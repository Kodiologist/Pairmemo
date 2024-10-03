cache.env = new.env(parent = emptyenv())

define = \(f, directory, mem = F, format = "rds", ap = NULL, n.frame = 1L)
   {f = substitute(f)
    stopifnot(length(f) == 3 && identical(f[[1]], as.symbol("<-")))
    f.name = deparse(f[[2]])
    pos = parent.frame(n.frame)
    f = eval(f[[3]], pos)

    stopifnot(!missing(directory))
    kcache = NULL
    vcache = NULL
    initialized = F
    init = \()
      # Initialize the directory and the memory cache.
       {if (initialized)
            return()

        if (!is.character(directory))
            directory <<- directory()
        # `directory` is assumed to already exist, but we'll create
        # its per-function subdirectory that we're going to use if it
        # doesn't already exist.
        if (!dir.exists(directory))
            stop("The specified directory does not exist: ", directory)
        directory <<- file.path(directory, f.name)

        if (mem)
          # The memory cache uses environments instead of lists so we
          # get pass-by-reference semantics.
           {cache.env[[directory]] = new.env(parent = emptyenv())
            cache.env[[directory]]$kcache = new.env(parent = emptyenv())
            cache.env[[directory]]$vcache = new.env(parent = emptyenv())
            kcache <<- cache.env[[directory]]$kcache
            vcache <<- cache.env[[directory]]$vcache}

        initialized <<- T}

    # Define a new function and set it to `f.name` in the calling
    # scope.
    assign(f.name, pos = pos, \(...)
       {init()

        # Process the arguments.
        args = sys.call()
        return.kv = F
        if (length(names(args)) && names(args)[2] == "PAIRMEMO.KV")
          # This argument is meant for us rather than for the wrapped
          # function. Remove it and set a flag.
           {return.kv = eval(args[[2]], parent.frame())
            args = args[-2]}
        # Standardize the arguments by using `match.call`.
        args = lapply(match.call(f, args)[-1], eval, envir = parent.frame())
        # Alphabetize the argument names. This should prevent merely
        # reordering a parameter list from changing cache keys.
        if (length(names(args)))
            args = c(
                args[sort(names(args)[names(args) != ""])],
                args[names(args) == ""])
        # Apply any user-provided argument processing in `ap`.
        params = formals(f)
        for (pn in names(ap))
            {stopifnot(pn %in% names(params))
             if (pn %in% names(args))
                 args[[pn]] = ap[[pn]](args[[pn]])}
        # Remove arguments that are set to their default values,
        # unless there's a "..." parameter, in which case arguments
        # might get misassigned if we do this.
        #
        # We only evaluate default values that are constant
        # expressions.
        if (!("..." %in% names(params)))
            for (pn in names(params))
                {if (!(pn %in% names(args)) ||
                         identical(params[[pn]], substitute()))
                     next
                 default.value = tryCatch(
                     eval.const(bquote(list(.(params[[pn]])))),
                     error = \(e) e)
                 if (!length(default.value) ||
                         !identical(default.value[[1]], args[[pn]]))
                     next
                 args[[pn]] = NULL}
        if (!length(args))
          # If the list is empty, make sure it's a plain list, since
          # empty named and plain lists hash differently.
            args = list()
        key = list(args = args)
        hash = paste0("h", digest::digest(key, algo = "xxhash64"))

        # Check the memory cache.
        if (mem && exists(hash, vcache))
            return(vcache[[hash]])

        # Check the file cache.
        path = file.path(directory, hash)
        if (!file.exists(paste0(path, ".json")))
          # It's a total cache miss, so actually call the function.
           {t1 = proc.time()
            v = do.call(f, key$args)
            t2 = proc.time()

            dir.create(directory, showWarnings = F)
            suppressPackageStartupMessages(
                get.format(format)$write(v, path))
            write(file = paste0(path, ".json"),
                jsonlite::toJSON(auto_unbox = T, digits = NA, c(
                    list(
                        file_format = get.format(format)$name,
                        time = unname(t2["elapsed"] - t1["elapsed"])),
                    key)))}

        # Read the saved value. (We do this even if we just wrote it,
        # to ensure that we return the same value we would return on a
        # cache hit.)
        v = suppressPackageStartupMessages(
             get.format(format)$read(path))
        if (mem || return.kv)
            key = jsonlite::fromJSON(simplifyVector = F,
                paste0(path, ".json"))

        # Update the memory cache.
        if (mem)
           {kcache[[hash]] = key
            vcache[[hash]] = v}

        # Return the function value.
        if (return.kv)
            list(k = key, v = v)
        else
            v})}

path2hash = \(path)
    tools::file_path_sans_ext(regmatches(path,
        regexpr("([a-z0-9]+)\\.json$", path)))

metadata = \(f, filter = \(x) TRUE)
   {fe = environment(f)
    fe$init()
    paths = list.files(fe$directory, pattern = "\\.json$", full.names = T)
    l =
       {if (fe$mem)
          # Check for any new entries, and load the corresponding
          # JSON files into the memory cache.
           {for (path in paths[!(
                    path2hash(paths) %in% ls(fe$kcache))])
                fe$kcache[[path2hash(path)]] =
                    jsonlite::fromJSON(path, simplifyVector = F)
            as.list(fe$kcache)}
        else
            `names<-`(lapply(paths, jsonlite::fromJSON, simplifyVector = F),
                path2hash(paths))}
    if (!length(l))
        return(l)
    # Apply the filter manually instead of with `Filter`, so we
    # can treat 0-length results as false and throw an error for
    # results of length greater than 2.
    ix = sapply(l, \(item)
        {y = filter(item)
         if (length(y) == 0)
             F
         else if (length(y) == 1)
             if (is.na(y))
                 F
             else
                 as.logical(y)
         else
             stop("Filter returned vector of length ", length(y))})
    stopifnot(length(ix) == length(l))
    l[which(ix)]}

kvs = \(f, filter = \(x) TRUE)
   {keys = pairmemo::metadata(f, filter)
    fe = environment(f)
    l = lapply(names(keys), \(hash) list(
        k = keys[[hash]],
        v =
           {if (fe$mem && exists(hash, fe$vcache))
                v = fe$vcache[[hash]]
            else
               {v = suppressPackageStartupMessages(
                    get.format(fe$format)$read(
                         file.path(fe$directory, hash)))
                if (fe$mem)
                    fe$vcache[[hash]] = v}
            v}))
    `names<-`(l, names(keys))}

clear = \(f, filter = \(x) TRUE)
   {fe = environment(f)
    fe$init()
    deleted = 0L
    for (hash in names(pairmemo::metadata(f, filter)))
       {file.remove(paste0(file.path(fe$directory, hash), ".json"))
        file.remove(file.path(fe$directory, hash))
        if (fe$mem)
           {rm(list = hash, pos = fe$kcache)
            if (exists(hash, fe$vcache))
                rm(list = hash, pos = fe$vcache)}
        deleted = deleted + 1L}
    c("Cache entries deleted" = deleted)}

builtin.formats = list(
    rds = list(
        read = \(path) readRDS(path),
        write = \(v, path) saveRDS(v, path)),
    qs = list(
        read = \(path) qs::qread(path),
        write = \(v, path) qs::qsave(v, path)),
    fst = list(
        read = \(path) fst::read.fst(path, as.data.table = T),
        write = \(v, path) fst::write.fst(v, path)))
local(
    for (name in names(builtin.formats))
        builtin.formats[[name]]$name <<- name)
get.format = \(format)
   {if (is.character(format))
        builtin.formats[[format]]
    else
        format}
