#include <testthat.h>
#include "dijkstra.h"


DijkstraGraph initGraph(int rows, int cols, int layers) {
  NumericVector d(rows * cols * layers * 6);
  for (int i = 0; i < d.length(); ++i) {
    d[i] = 10 * i;
  }
  NumericVector t(rows * cols * layers * (layers - 1));
  for (int i = 0; i < t.length(); ++i) {
    t[i] = 100 * i;
  }
  DijkstraGraph g(d, t, rows, cols, layers);
  return g;
}


context("dijkstra") {
  test_that("DijkstraQueueElement priority comparison") {
    DijkstraQueueElement e1 = DijkstraQueueElement(3, 10);
    DijkstraQueueElement e2 = DijkstraQueueElement(3, 10);
    DijkstraQueueElement e3 = DijkstraQueueElement(3, 11);
    DijkstraQueueElement e4 = DijkstraQueueElement(4, 10);

    expect_false(e1 < e1);

    expect_false(e1 < e2);
    expect_false(e2 < e1);

    expect_true(e3 < e1);
    expect_false(e1 < e3);

    expect_true(e1 < e4);
    expect_false(e4 < e1);
  }


  test_that("DijkstraGraph::getDirections()") {
    DijkstraGraph g = initGraph(4, 3, 2);
    expect_true(g.getDirections() == 6 + 2 - 1);
  }


  test_that("DijkstraGraph::layerIndices()") {
    int source, target = 0;
    DijkstraGraph g = initGraph(4, 3, 2);

    for (int dir = 0; dir < 6; ++dir) {
      g.layerIndices(5, dir, source, target);
      expect_true(source == 0);
      expect_true(target == 0);
      g.layerIndices(17, dir, source, target);
      expect_true(source == 1);
      expect_true(target == 1);
    }

    g.layerIndices(5, 6, source, target);
    expect_true(source == 0);
    expect_true(target == 1);

    g.layerIndices(17, 6, source, target);
    expect_true(source == 1);
    expect_true(target == 0);
  }

  test_that("DijkstraGraph::layerIndices() 2") {
    int source, target = 0;
    DijkstraGraph g = initGraph(4, 3, 3);

    for (int dir = 0; dir < 6; ++dir) {
      g.layerIndices(5, dir, source, target);
      expect_true(source == 0);
      expect_true(target == 0);
      g.layerIndices(17, dir, source, target);
      expect_true(source == 1);
      expect_true(target == 1);
      g.layerIndices(29, dir, source, target);
      expect_true(source == 2);
      expect_true(target == 2);
    }

    g.layerIndices(5, 6, source, target);
    expect_true(source == 0);
    expect_true(target == 1);

    g.layerIndices(5, 7, source, target);
    expect_true(source == 0);
    expect_true(target == 2);

    g.layerIndices(17, 6, source, target);
    expect_true(source == 1);
    expect_true(target == 0);

    g.layerIndices(17, 7, source, target);
    expect_true(source == 1);
    expect_true(target == 2);

    g.layerIndices(29, 6, source, target);
    expect_true(source == 2);
    expect_true(target == 0);

    g.layerIndices(29, 7, source, target);
    expect_true(source == 2);
    expect_true(target == 1);
  }

  test_that("DijkstraGraph::neighbour()") {
    DijkstraGraph g = initGraph(4, 3, 3);

    expect_true(g.neighbour(5, 0) == 0);
    expect_true(g.neighbour(5, 1) == 4);
    expect_true(g.neighbour(5, 2) == 9);
    expect_true(g.neighbour(5, 3) == 6);
    expect_true(g.neighbour(5, 4) == 2);
    expect_true(g.neighbour(5, 5) == 1);
    expect_true(g.neighbour(5, 6) == 17);
    expect_true(g.neighbour(5, 7) == 29);

    expect_true(g.neighbour(17, 0) == 0 + 12);
    expect_true(g.neighbour(17, 1) == 4 + 12);
    expect_true(g.neighbour(17, 2) == 9 + 12);
    expect_true(g.neighbour(17, 3) == 6 + 12);
    expect_true(g.neighbour(17, 4) == 2 + 12);
    expect_true(g.neighbour(17, 5) == 1 + 12);
    expect_true(g.neighbour(17, 6) == 5);
    expect_true(g.neighbour(17, 7) == 29);

    expect_true(g.neighbour(29, 0) == 0 + 24);
    expect_true(g.neighbour(29, 1) == 4 + 24);
    expect_true(g.neighbour(29, 2) == 9 + 24);
    expect_true(g.neighbour(29, 3) == 6 + 24);
    expect_true(g.neighbour(29, 4) == 2 + 24);
    expect_true(g.neighbour(29, 5) == 1 + 24);
    expect_true(g.neighbour(29, 6) == 5);
    expect_true(g.neighbour(29, 7) == 17);
  }

  test_that("DijkstraGraph::neighbour() nonexistent") {
    DijkstraGraph g = initGraph(4, 3, 2);
    expect_true(g.neighbour(0, 0) == -1);
    expect_true(g.neighbour(0, 1) == -1);
    expect_true(g.neighbour(0, 5) == -1);

    expect_true(g.neighbour(4, 0) == -1);
    expect_true(g.neighbour(4, 1) == -1);

    expect_true(g.neighbour(8, 0) == -1);
    expect_true(g.neighbour(8, 1) == -1);
    expect_true(g.neighbour(8, 2) == -1);
    expect_true(g.neighbour(8, 3) == -1);

    expect_true(g.neighbour(9, 2) == -1);

    expect_true(g.neighbour(10, 1) == -1);
    expect_true(g.neighbour(10, 2) == -1);
    expect_true(g.neighbour(10, 3) == -1);

    expect_true(g.neighbour(11, 2) == -1);
    expect_true(g.neighbour(11, 3) == -1);
    expect_true(g.neighbour(11, 4) == -1);

    expect_true(g.neighbour(7, 3) == -1);
    expect_true(g.neighbour(7, 4) == -1);

    expect_true(g.neighbour(3, 0) == -1);
    expect_true(g.neighbour(3, 3) == -1);
    expect_true(g.neighbour(3, 4) == -1);
    expect_true(g.neighbour(3, 5) == -1);

    expect_true(g.neighbour(2, 5) == -1);

    expect_true(g.neighbour(1, 0) == -1);
    expect_true(g.neighbour(1, 4) == -1);
    expect_true(g.neighbour(1, 5) == -1);
    //----
    expect_true(g.neighbour(12 + 0, 0) == -1);
    expect_true(g.neighbour(12 + 0, 1) == -1);
    expect_true(g.neighbour(12 + 0, 5) == -1);

    expect_true(g.neighbour(12 + 4, 0) == -1);
    expect_true(g.neighbour(12 + 4, 1) == -1);

    expect_true(g.neighbour(12 + 8, 0) == -1);
    expect_true(g.neighbour(12 + 8, 1) == -1);
    expect_true(g.neighbour(12 + 8, 2) == -1);
    expect_true(g.neighbour(12 + 8, 3) == -1);

    expect_true(g.neighbour(12 + 9, 2) == -1);

    expect_true(g.neighbour(12 + 10, 1) == -1);
    expect_true(g.neighbour(12 + 10, 2) == -1);
    expect_true(g.neighbour(12 + 10, 3) == -1);

    expect_true(g.neighbour(12 + 11, 2) == -1);
    expect_true(g.neighbour(12 + 11, 3) == -1);
    expect_true(g.neighbour(12 + 11, 4) == -1);

    expect_true(g.neighbour(12 + 7, 3) == -1);
    expect_true(g.neighbour(12 + 7, 4) == -1);

    expect_true(g.neighbour(12 + 3, 0) == -1);
    expect_true(g.neighbour(12 + 3, 3) == -1);
    expect_true(g.neighbour(12 + 3, 4) == -1);
    expect_true(g.neighbour(12 + 3, 5) == -1);

    expect_true(g.neighbour(12 + 2, 5) == -1);

    expect_true(g.neighbour(12 + 1, 0) == -1);
    expect_true(g.neighbour(12 + 1, 4) == -1);
    expect_true(g.neighbour(12 + 1, 5) == -1);
  }

  test_that("DijkstraGraph::opositeDirection()") {
    DijkstraGraph g = initGraph(4, 3, 3);

    expect_true(g.oppositeDirection(5, 0) == 3);
    expect_true(g.oppositeDirection(5, 1) == 4);
    expect_true(g.oppositeDirection(5, 2) == 5);
    expect_true(g.oppositeDirection(5, 3) == 0);
    expect_true(g.oppositeDirection(5, 4) == 1);
    expect_true(g.oppositeDirection(5, 5) == 2);
    expect_true(g.oppositeDirection(5, 6) == 6);
    expect_true(g.oppositeDirection(5, 7) == 6);

    expect_true(g.oppositeDirection(17, 0) == 3);
    expect_true(g.oppositeDirection(17, 1) == 4);
    expect_true(g.oppositeDirection(17, 2) == 5);
    expect_true(g.oppositeDirection(17, 3) == 0);
    expect_true(g.oppositeDirection(17, 4) == 1);
    expect_true(g.oppositeDirection(17, 5) == 2);
    expect_true(g.oppositeDirection(17, 6) == 6);
    expect_true(g.oppositeDirection(17, 7) == 7);

    expect_true(g.oppositeDirection(29, 0) == 3);
    expect_true(g.oppositeDirection(29, 1) == 4);
    expect_true(g.oppositeDirection(29, 2) == 5);
    expect_true(g.oppositeDirection(29, 3) == 0);
    expect_true(g.oppositeDirection(29, 4) == 1);
    expect_true(g.oppositeDirection(29, 5) == 2);
    expect_true(g.oppositeDirection(29, 6) == 7);
    expect_true(g.oppositeDirection(29, 7) == 7);
  }

  test_that("DijkstraGraph::edgePrice()") {
    DijkstraGraph g = initGraph(4, 3, 3);

    expect_true(g.edgePrice(5, 0) == 50);
    expect_true(g.edgePrice(5, 1) == 410);
    expect_true(g.edgePrice(5, 2) == 770);
    expect_true(g.edgePrice(5, 3) == 1130);
    expect_true(g.edgePrice(5, 4) == 1490);
    expect_true(g.edgePrice(5, 5) == 1850);
    expect_true(g.edgePrice(5, 6) == 500);
    expect_true(g.edgePrice(5, 7) == 1700);

    expect_true(g.edgePrice(17, 0) == 170);
    expect_true(g.edgePrice(17, 1) == 530);
    expect_true(g.edgePrice(17, 2) == 890);
    expect_true(g.edgePrice(17, 3) == 1250);
    expect_true(g.edgePrice(17, 4) == 1610);
    expect_true(g.edgePrice(17, 5) == 1970);
    expect_true(g.edgePrice(17, 6) == 2900);
    expect_true(g.edgePrice(17, 7) == 4100);

    expect_true(g.edgePrice(29, 0) == 290);
    expect_true(g.edgePrice(29, 1) == 650);
    expect_true(g.edgePrice(29, 2) == 1010);
    expect_true(g.edgePrice(29, 3) == 1370);
    expect_true(g.edgePrice(29, 4) == 1730);
    expect_true(g.edgePrice(29, 5) == 2090);
    expect_true(g.edgePrice(29, 6) == 5300);
    expect_true(g.edgePrice(29, 7) == 6500);
  }
}
