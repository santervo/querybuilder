(ns querybuilder.test.core
  (:use [querybuilder.core] :reload)
  (:use [clojure.test]))

(deftest test-from
  (is (= {:from "post" :select {"post" all}}
         (from :post))))

(deftest test-modify-select
  (testing "specify cols as map"
    (is (= {"post" {"comment_title" "title" "comment_body" "body"}}
           (:select (-> (from :post) (modify-select {:post {:comment_title :title :comment_body :body}})))))))

