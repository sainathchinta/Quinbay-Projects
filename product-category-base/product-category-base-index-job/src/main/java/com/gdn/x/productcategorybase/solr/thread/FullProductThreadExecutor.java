package com.gdn.x.productcategorybase.solr.thread;

import com.gdn.x.productcategorybase.solr.model.ProductModel;
import com.gdn.x.productcategorybase.solr.model.ReIndexType;
import com.gdn.x.productcategorybase.solr.service.ProductService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.List;
import java.util.Map;

/**
 * Created by Kesha on 25/05/16.
 */
public class FullProductThreadExecutor extends ProductThreadExecutor implements Runnable {

  private List<ProductModel> productModelList;
  private static final Logger LOGGER = LoggerFactory.getLogger(FullProductThreadExecutor.class);

  public FullProductThreadExecutor(ProductService productService, Map<String, String>
      categoryToFinalParentMap, List<ProductModel> productModelList) {
    super(productService, categoryToFinalParentMap);
    this.productModelList = productModelList;
  }

  @Override
  public void run() {
    LOGGER.info("Full Reindex: Thread {} started to post {} documents",
        Thread.currentThread().getName(), productModelList.size());
    try {
      getProductService().getDataAndPostDocumentsToSolr(productModelList,
          getCategoryToFinalParentMap(), ReIndexType.FULL);

    } catch (Exception e) {
      LOGGER.error("Error occurred executing full product reindex while executing thread {}",
          Thread.currentThread().getName(), e);
    }
  }

  public void cleanup() {
    super.cleanup();
    this.productModelList = null;
  }
}
