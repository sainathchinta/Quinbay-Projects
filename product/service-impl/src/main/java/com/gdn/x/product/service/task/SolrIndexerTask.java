package com.gdn.x.product.service.task;

import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.gdn.common.base.GdnObjects;
import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.model.entity.Product;
import com.gdn.x.product.model.vo.MasterDataDetailWithProductAndItemsResponseVo;
import com.gdn.x.product.model.vo.ProductAndItemsVO;
import com.gdn.x.product.service.api.ProductAndItemSolrIndexerService;
import com.gdn.x.product.service.api.ProductService;

public class SolrIndexerTask implements Runnable {

  private static final Logger LOGGER = LoggerFactory.getLogger(SolrIndexerTask.class);

  private ProductAndItemSolrIndexerService productAndItemIndexService;

  private ProductService productService;

  private Object dataTobeIndexed;

  public SolrIndexerTask() {
    // do nothing
  }

  public SolrIndexerTask(ProductAndItemSolrIndexerService productAndItemIndexService,
      Object dataTobeIndexed) {
    super();
    this.productAndItemIndexService = productAndItemIndexService;
    this.dataTobeIndexed = dataTobeIndexed;
  }

  public SolrIndexerTask(ProductAndItemSolrIndexerService productAndItemIndexService,
      ProductService productService, Object dataTobeIndexed) {
    super();
    this.productAndItemIndexService = productAndItemIndexService;
    this.dataTobeIndexed = dataTobeIndexed;
    this.productService = productService;
  }

  private void doIndexItem(String productSku, List<Item> itemList) {
    try {
      if (itemList != null && !itemList.isEmpty()) {
        String storeId = itemList.get(0).getStoreId();
        Product product = this.productService
            .findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(storeId, productSku);
        this.productAndItemIndexService
            .applyProductAndItems(new ProductAndItemsVO(product, itemList), false);
      }
    } catch (Exception e) {
      SolrIndexerTask.LOGGER.error("failure on indexing solr document {} ", this.dataTobeIndexed,
          e);
    }
  }

  @Override
  public boolean equals(Object obj) {
    return GdnObjects.equals(this, obj);
  }

  private Class<?> getClassType(List<Object> objects) {
    return objects.stream().map(o -> o.getClass()).findFirst().get();
  }

  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
  }

  private void indexItem(List<Item> listOfItems) {
    Map<String, List<Item>> mapOfItems =
        listOfItems.stream().collect(Collectors.groupingBy(e -> e.getProductSku()));
    for (String productSku : mapOfItems.keySet()) {
      List<Item> items = mapOfItems.get(productSku);
      doIndexItem(productSku, items);
    }
  }

  @SuppressWarnings("unchecked")
  private void indexListOfObject(Object objectTobeIndexed) throws Exception {
    List<Object> objects = (List<Object>) objectTobeIndexed;
    Class<?> classType = getClassType(objects);
    if (Item.class.isAssignableFrom(classType)) {
      this.indexItem((List<Item>) objectTobeIndexed);
    } else {
      objects.forEach(this::indexObject);
    }
  }

  private void indexObject(Object dataTobeIndexed) {
    try {
      SolrIndexerTask.LOGGER.debug("start indexing data : {}", dataTobeIndexed);
      if (dataTobeIndexed instanceof Item) {
        Item item = (Item) dataTobeIndexed;
        this.doIndexItem(item.getProductSku(), Arrays.asList(item));
      } else if (dataTobeIndexed instanceof Product) {
        this.productAndItemIndexService.applyProduct((Product) dataTobeIndexed, false);
      } else if (dataTobeIndexed instanceof ProductAndItemsVO) {
        this.productAndItemIndexService.applyProductAndItems((ProductAndItemsVO) dataTobeIndexed, false);
      } else if (dataTobeIndexed instanceof MasterDataDetailWithProductAndItemsResponseVo) {
        this.productAndItemIndexService.applyMasterDataDetailWithProductAndItems(
            (MasterDataDetailWithProductAndItemsResponseVo) dataTobeIndexed, false);
      }
    } catch (Exception e) {
      SolrIndexerTask.LOGGER.error("failure on indexing solr document {} ", this.dataTobeIndexed,
          e);
    }
  }

  @Override
  public void run() {
    try {
      if (this.dataTobeIndexed == null) {
        return;
      }
      if (this.dataTobeIndexed instanceof List<?>) {
        this.indexListOfObject(this.dataTobeIndexed);
      } else {
        this.indexObject(this.dataTobeIndexed);
      }
    } catch (Exception e) {
      SolrIndexerTask.LOGGER.error("failure on indexing solr document {} ", this.dataTobeIndexed,
          e);
    }
  }

  public void setDataTobeIndexed(Object dataTobeIndexed) {
    this.dataTobeIndexed = dataTobeIndexed;
  }

  public void setProductAndItemIndexService(
      ProductAndItemSolrIndexerService productAndItemIndexService) {
    this.productAndItemIndexService = productAndItemIndexService;
  }

}
