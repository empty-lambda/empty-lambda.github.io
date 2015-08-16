---
layout: post
title: "Simple Parallel haskell [未完成]"
description: ""
category: [Programming]
tags: [Haskell, Parallelism]
---
{% include JB/setup %}

Originated from one parallelism [problem](http://mail.haskell.org/pipermail/haskell-cafe/2015-August/120872.html "article") found in Haskell-Cafe mailing list.  
順便寫下一些 Haskell Parallelism 中文化的文檔 （剛好上學期上完休斯大大的課啊）  
Parallelism 的中文方面好像有點缺乏，連主題所用的名詞該怎麼翻譯成中文都會覺得有些許不完善處；  

[這篇](http://wilberchao.blogspot.se/2014/01/concurrency-vs-parallel-programming.html)來自 Scala的文中談到 Concurrency 與 Parallelism 的名詞釋義，尤其贊同前言部分所分享的感覺（在Terminology 上所花費的記憶與分辨努力在後期會大大的助益進階學習中常見的跨主題研究），這點從很久以前就有意識到中文翻譯在 computer science 領域上的不足處，近期接觸到更多數學根基為背景的程式理論後更覺得重要！  

先花點時間來分辨這兩個詞吧：

- Concurrency :  
	就我自己學完 PFP 之後對 Concurrency 的定義大概可以用“一致性”來翻譯，為了維持資料或資源的一致性，在多方共享前提下（此處多方並不一定只多核處理器，而是更加廣泛的多方，諸如多個執行序列或是在系統內多個不同的硬體: e.g. {滑鼠, 鍵盤, 隨身碟} 共享 USB port 的 bus）所需要維持一致性與盡可能達到公平性所設計之機構。  這樣的一致性即便在單核處理器環境下也是必要的！  

	multi-threading 的程式就算在單核下也要能夠執行，你總不會希望把所有視窗上的小工具都分給一個單獨核心來運作，就算真的都是單獨核心運算的架構下也無法避免遇到競爭共享資源，比方說顯示器的 driver 程式，這樣的模型會被我連結到基礎電訊的 multiplexing 原理，就像幾十年前的手動轉接電話那種系統。自然會有很多源於電訊的理論可以使用在這樣的問題中。  

	可以試想在零組件生產線上的工作站之間必須要達成某種程度上的一致性（對於共用的工具或資源），才不會造成一個工作站為了等其他工作站而閒置（empty-spining）
- Parallelism :  
	對於 Parallelism 我會喜歡翻譯為“分工”，延續前面工作生產線的例子，這種分工可以視為同一工作站內多名工作人員做著一樣的事情（或者不一樣但是互補的部分），這樣就不難觀察到在“分工”裡也有可能遭遇到“一致性”的問題！（把每個作業人員視為一個工作站）。  

	不管他們是做同樣的工作或是互補的工作（有無相依性）都有可能會需要解決 Concurrency 上的問題（共用的螺絲起子），前文所引述的 Scala 文章中有說到：
	> 因此Concurrency在Multiple cores CPU中就有Parallel programming特性
	這點我不盡同意，為了能夠達成 Parallelism，必須先思考有沒有辦法能夠對生產效率進行提升！（不是狂請一百個工人就會有一百倍效率）  
	
	在我的想法裡，Concurrency 只不過是要達成 Parallelism 前的必要條件，而非充要條件，這也是真正 parallelism 麻煩的地方（不然光就一致性的達成我們已經有許多電訊方面的理論可以使用了）

	> 分工一定是屬於合作的一種，但分工本身不一定會帶來效益；不恰當的分工反而造成工作效率低落

	不恰當的分工可以是 “過度分工” （分工過細，光是安排分工的手續就遠比單位工作內容多太多）或是 “不等分分工” （我解八題高等微積分你練習寫下八個國字）這些都是不良的分工，最糟的狀態是分工後比分工前需要更多時間（最糟不是與單一工作 par 而是更糟）。  
	而想要達到有效率的分工，我們必須先瞭解這些工人們的特性（每個工人能力是否相同？ 其工作的效率？等等）

---------

	import Control.Parallel.Strategies
	import Control.DeepSeq

	import Data.Ratio

	divConq :: (NFData b) => (a -> b)
	        -> a
	        -> (a -> Bool)
	        -> (b -> b -> b)
	        -> (a -> Maybe (a,a))
	        -> b
	divConq f arg threshold conquer divide = go arg
	    where
	      go arg =
	          case divide arg of
	            Nothing -> f arg
	            Just (l0,r0) -> conquer l1 r1 `using` strat
	                where
	                  l1 = go l0
	                  r1 = go r0
	                  strat x = do r l1; r r1; return x
	                      where r | threshold arg = rdeepseq
	                              | otherwise     = rpar

	pqCombine :: (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer)
	pqCombine (pl, ql) (pr, qr) = (pl*qr+pr, ql*qr)

	pq :: (Integer, Integer) -> (Integer, Integer)
	pq (a, b)  = (\t -> (sum t, last t)) $ scanl1 (*) [b,b-1..a+1]

	euler :: Integer -> Rational
	euler n =
	    let (p,q) = divConq pq
	                        (0,n)
	                        (\(a,b) -> b-a < 10000)
	                        pqCombine
	                        (\(a,b) -> if b-a > 5
	                                   then let m = (a+b+1) `div` 2 in Just
	((a,m), (m, b))
	                                   else Nothing)
	    in p%q

	main = print $ euler 320000 `seq` ()

----------

	> ./BinSplit +RTS -s -N1
	()
	     178,375,880 bytes allocated in the heap
	       2,452,040 bytes copied during GC
	       3,222,696 bytes maximum residency (7 sample(s))
	         883,040 bytes maximum slop
	              11 MB total memory in use (2 MB lost due to fragmentation)

	                                     Tot time (elapsed)  Avg pause  Max
	pause
	  Gen  0       333 colls,     0 par    0.004s   0.002s     0.0000s
	 0.0000s
	  Gen  1         7 colls,     0 par    0.001s   0.001s     0.0001s
	 0.0003s

	  TASKS: 4 (1 bound, 3 peak workers (3 total), using -N1)

	  SPARKS: 126 (0 converted, 0 overflowed, 0 dud, 0 GC'd, 126 fizzled)

	  INIT    time    0.001s  (  0.000s elapsed)
	  MUT     time    0.928s  (  0.936s elapsed)
	  GC      time    0.005s  (  0.003s elapsed)
	  EXIT    time    0.001s  (  0.000s elapsed)
	  Total   time    0.935s  (  0.939s elapsed)

	  Alloc rate    192,215,387 bytes per MUT second

	  Productivity  99.4% of total user, 98.9% of total elapsed


	> ./BinSplit +RTS -s -N4
	()
	     178,727,480 bytes allocated in the heap
	       3,506,488 bytes copied during GC
	       3,650,032 bytes maximum residency (7 sample(s))
	         934,976 bytes maximum slop
	              12 MB total memory in use (1 MB lost due to fragmentation)

	                                     Tot time (elapsed)  Avg pause  Max
	pause
	  Gen  0       141 colls,   141 par    0.009s   0.002s     0.0000s
	 0.0001s
	  Gen  1         7 colls,     6 par    0.003s   0.001s     0.0001s
	 0.0001s

	  Parallel GC work balance: 38.80% (serial 0%, perfect 100%)

	  TASKS: 10 (1 bound, 9 peak workers (9 total), using -N4)

	  SPARKS: 126 (12 converted, 0 overflowed, 0 dud, 0 GC'd, 114 fizzled)

	  INIT    time    0.002s  (  0.002s elapsed)
	  MUT     time    2.104s  (  0.946s elapsed)
	  GC      time    0.012s  (  0.003s elapsed)
	  EXIT    time    0.001s  (  0.000s elapsed)
	  Total   time    2.119s  (  0.951s elapsed)

	  Alloc rate    84,946,520 bytes per MUT second

	  Productivity  99.3% of total user, 221.3% of total elapsed

