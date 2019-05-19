package com.github.study.btree

import org.scalatest._

class BTreeSpec extends FunSpec with Matchers {

  describe("BTree") {

    describe("Create BTree with Leaf and Branch combination") {

      val bTree1 = BTree(Leaf(1))
      val bTree2 = BTree(Branch(Leaf(1), 2, Leaf(3)))
      val bTree3 = BTree(Branch(Branch(Leaf(1), 2, Leaf(3)), 4, Branch(Leaf(5), 6, Leaf(7))))

      println(bTree1)
      println(bTree2)
      println(bTree3)

      it("should compute size of BTree") {
        bTree1.size should be(1)
        bTree2.size should be(3)
        bTree3.size should be(7)
      }

      it("should compute max value of BTree") {
        bTree1.max should be(1)
        bTree2.max should be(3)
        bTree3.max should be(7)
      }

      it("should compute min value of BTree") {
        bTree1.min should be(1)
        bTree2.min should be(1)
        bTree3.min should be(1)
      }

      it("should compute sum value of BTree") {
        bTree1.sum should be(1)
        bTree2.sum should be(6)
        bTree3.sum should be(28)
      }

      it("should compute avg value of BTree") {
        bTree1.avg should be(1)
        bTree2.avg should be(2)
        bTree3.avg should be(4)
      }

      it("should create BTree from list") {
        BTree(List(1)) should be(bTree1)
        BTree(List(1, 2, 3)) should be(bTree2)
        BTree(List(1, 2, 3, 4, 5, 6, 7)) should be(bTree3)
      }

      it("should define new BTree with previous BTree nodes") {
        val bTree3mid = 8
        val bTree3r = BTree((9 to 15).toList)
        val bTree4 = BTree(Branch(bTree3.node, bTree3mid, bTree3r.node))

        println(bTree3r)
        println(bTree4)

        BTree((1 to 15).toList) should be(bTree4)
      }

    }

  }

}
