---
title: an unsafeInterleaveIO example
date: August 10, 2011
---

:PostID: 150
:Title: an unsafeInterleaveIO example
:Keywords: ghc, haskell, example, unsafe
:Categories: notes

Этот пост будет про `unsafeInterleaveIO <http://www.haskell.org/ghc/docs/latest/html/libraries/base/System-IO-Unsafe.html#v:unsafeInterleaveIO>`_.

Часто хочется работать с **IO** данными, будто они являются ленивыми.
Как **hGetContents** (или **readFile**), которая возвращает ленивую строку-содержание файла:

.. code-block:: haskell

    do by_line <- lines `fmap` readFile "/some/log/file"
       -- дальше работает с by_line как с обычным ленивым списком

По мере необходимости при использваонии новых строк они считываются с диска,
а чтобы файл закрылся - надо обработать все данные (с этим надо аккуратно).

Попробуем построить похожую функцию, которая получает данные из **IO** по мере использования.

.. raw:: html

   <!--more-->

Примером у нас будет ленивое дерево файловой системы. Будем получать и тут же выводить
имена файлов в каталогах полностью не получив всё дерево.

.. code-block:: haskell

    module Main (main) where
    --
    import Control.Monad
    import Prelude as E -- exceptions only
    import System.IO.Unsafe (unsafeInterleaveIO)
    import System.Directory
    import System.FilePath
    import System.Posix.Files
    import Text.Printf
    --
    data Node = File String -- file_name
              | Dir  String [Node] -- dir_name children
    --
    show_tree :: (String, Int) -> Node -> IO ()
    show_tree (prefix, depth) tree =
      case tree of
          File n   -> printf "%s%s\n" prefix n
          Dir  n c -> do printf "%s%s/ {children=%u}\n" prefix n (length c)
                         if depth > 1 then forM_ c $ show_tree ("    " ++ prefix, depth - 1)
                                      else putStrLn $ "    " ++ prefix ++ "{ ... }"
    --
    nothrow :: IO a -> IO ()
    nothrow action = E.catch (action >> return ()) $
                         \xc -> putStrLn $ "ERROR: I caught an XC: " ++ show xc
    --
    main :: IO ()
    main =
        do putStrLn "1. Strict tree:"
           nothrow $ strictTree "/etc" >>= show_tree ("  ", 2)
           putStrLn "2.*** Lazy tree:"
           nothrow $ lazyTree   "/etc" >>= show_tree ("  ", 2)

Ничего особенного кроме того, что реализации **strictTree/lazyTree** пока нет.
Я ее специально оттянул отдельно, чтобы не так страшно было.

Берем список каталогов **/etc/** и распечанываем первые 2 уровня.
- **strictTree** получает весь список
- **lazyTree** получает данные по мере их вывода в **show_tree**

.. code-block:: haskell

    -- ======================= --
    -- Implementation details:
    -- modes for 'getTree'.
    --
    data EvalStrategy = Lazy | Strict
    --
    lazyTree :: FilePath -> IO Node
    lazyTree = getTree Lazy
    --
    strictTree :: FilePath -> IO Node
    strictTree = getTree Strict
    --
    eval_thunk :: EvalStrategy -> IO a -> IO a
    eval_thunk Strict = id -- don't delay computatin (nothing special)
    eval_thunk Lazy   = unsafeInterleaveIO -- postpone up to evaluation
    --
    getTree :: EvalStrategy -> FilePath -> IO Node
    getTree eval_mode path = eval_thunk eval_mode $ -- here comes the magic!
                                                    -- 'eval' or 'postpone'?
        do s <- getFileStatus path
           let basename = takeBaseName path
           case (isDirectory s) of
               False -> return $ File basename
               True -> getDirectoryContents path >>=
                           mapM ( getTree eval_mode
                                . (path </>))
                                . filter (`notElem` [".", ".."]
                                ) >>=
                                    \children -> return $ Dir basename children

**lazyTree** и **strictTree** имеют одну реализацию!
Вся фишка в **eval_thunk**: она и делает всю магию - откладывает вычисление до реального использования.
С этим нужно быть осторожным, так как в программе порядок выполнения операций **IO** теперь не так очевиден.

Проверим, как оно работает:

.. code-block:: bash

    $ runhaskell uili.hs
    1. Strict tree:
    ERROR: I caught an XC: /etc/cron.weekly: getDirectoryContents: permission denied (Permission denied)
    2.*** Lazy tree:
      etc/ {children=229}
          gimp/ {children=1}
              { ... }
          crontab
          genkernel
          proftpd/ {children=3}
              { ... }
          modules/ {children=3}
              { ... }
          mke2fs
          lynx
          ld.so.conf/ {children=1}
              { ... }
          kvm/ {children=2}
              { ... }
          lisp-config
          dhcpcd
          dmtab
          openmpi/ {children=3}
              { ... }
          bonobo-activation/ {children=1}
              { ... }
          locale
          screenrc
          ulogd
          slsh
          adobe/ {children=1}
              { ... }
          minicom/ {children=1}
              { ... }
          unixODBC/ {children=3}
              { ... }
          paludis/ {children=12}
              { ... }
          services
    ERROR: I caught an XC: /etc/cron.weekly: getDirectoryContents: permission denied (Permission denied)

- **strictTree** не вернула вообще ничего и завершилась исключением,
- **lazyTree** распечатала всё до первого проблемного каталога.
  Если проверить **strace**, мы увидим, что на файлы в каталогах, помеченных как '{ ... }'
  **stat** не вызывался.

Такие пироги :]