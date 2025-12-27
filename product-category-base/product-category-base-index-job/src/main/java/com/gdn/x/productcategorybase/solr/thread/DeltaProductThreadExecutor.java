package com.gdn.x.productcategorybase.solr.thread;

import com.gdn.x.productcategorybase.solr.model.DeltaProduct;
import com.gdn.x.productcategorybase.solr.service.ProductService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.List;
import java.util.Map;

/**
 * Created by Kesha on 25/05/16.
 */
public class DeltaProductThreadExecutor extends ProductThreadExecutor implements Runnable {
  private List<DeltaProduct> deltaProductList;
  private static final Logger LOGGER = LoggerFactory.getLogger(DeltaProductThreadExecutor.class);

  public DeltaProductThreadExecutor(ProductService productService, Map<String, String>
      categoryToFinalParentMap, List<DeltaProduct> deltaProductList) {
    super(productService, categoryToFinalParentMap);
    this.deltaProductList = deltaProductList;
  }

  @Override
  public void run() {
    LOGGER.info("Delta Update: Thread {} started to post {} documents", Thread.currentThread()
        .getName(), deltaProductList.size());
    try {
      getProductService().deltaUpdate(deltaProductList, getCategoryToFinalParentMap());
    } catch (Exception e) {
      LOGGER.error("Error occurred executing Delta index job while executing thread {}", Thread
          .currentThread().getName(), e);
    }
  }
}
