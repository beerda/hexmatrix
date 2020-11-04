#include <testthat.h>
#include "hexmatrix.h"


DijkstraGraph initDijkstraGraph(int rows, int cols, int layers) {
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

  test_that("DijkstraGraph::edgePrice()") {
    DijkstraGraph g = initDijkstraGraph(4, 3, 3);

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
