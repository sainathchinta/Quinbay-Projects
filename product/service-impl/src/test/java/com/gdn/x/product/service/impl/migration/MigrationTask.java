package com.gdn.x.product.service.impl.migration;

import java.util.Date;
import java.util.List;
import java.util.concurrent.Callable;
import java.util.concurrent.TimeUnit;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.gdn.x.product.dao.api.ItemRepository;
import com.gdn.x.product.dao.api.ProductDb2Repository;
import com.gdn.x.product.dao.api.ProductRepository;
import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.model.entity.Product;
import com.gdn.x.product.model.vo.ProductAndItemsVO;
import com.google.common.base.Stopwatch;

public class MigrationTask implements Callable<String> {
  private static final Logger LOGGER = LoggerFactory.getLogger(MigrationTask.class);
  private ProductDb2Repository productDb2Repository;

  private ProductRepository productRepository;

  private ItemRepository itemRepository;

  private String productSku;

  public MigrationTask() {
    super();
  }

  public MigrationTask(ProductDb2Repository productDb2Repository,
      ProductRepository productRepository, ItemRepository itemRepository, String productSku) {
    super();
    this.productDb2Repository = productDb2Repository;
    this.productRepository = productRepository;
    this.itemRepository = itemRepository;
    this.productSku = productSku;
  }


  @Override
  public String call() {
    Stopwatch stopwatch = Stopwatch.createStarted();
    long elapsedAll = stopwatch.elapsed(TimeUnit.NANOSECONDS);
    long elapsed = 0;
    String result = "success:" + this.productSku;
    try {
      MigrationTask.LOGGER.info("start migrating {} at {}", this.productSku, new Date());
      elapsed = stopwatch.elapsed(TimeUnit.NANOSECONDS);
      Product product = this.productDb2Repository.findDb2ProductData(this.productSku);
      MigrationTask.LOGGER.info("findDb2ProductData : {}", stopwatch.elapsed(TimeUnit.NANOSECONDS)
          - elapsed);
      elapsed = stopwatch.elapsed(TimeUnit.NANOSECONDS);
      List<Item> items = this.productDb2Repository.findDb2ItemsData(product);
      MigrationTask.LOGGER.info("findDb2ItemsData : {}", stopwatch.elapsed(TimeUnit.NANOSECONDS)
          - elapsed);
      ProductAndItemsVO productAndItems = new ProductAndItemsVO(product, items);
      productAndItems = this.productDb2Repository.findDb2Attributes(productAndItems);
      if (!productAndItems.getProduct().isSynchronized()) {
        elapsed = stopwatch.elapsed(TimeUnit.NANOSECONDS);
        productAndItems = this.productDb2Repository.findDb2ProductImages(productAndItems);
        MigrationTask.LOGGER.info("findDb2ProductImages : {}",
            stopwatch.elapsed(TimeUnit.NANOSECONDS) - elapsed);
        elapsed = stopwatch.elapsed(TimeUnit.NANOSECONDS);
        MigrationTask.LOGGER.info("findDb2Attributes : {}", stopwatch.elapsed(TimeUnit.NANOSECONDS)
            - elapsed);
      }
      elapsed = stopwatch.elapsed(TimeUnit.NANOSECONDS);
      this.productRepository.save(productAndItems.getProduct());
      this.itemRepository.saveAll(productAndItems.getItems());
      MigrationTask.LOGGER.info("save : {}", stopwatch.elapsed(TimeUnit.NANOSECONDS) - elapsed);
      MigrationTask.LOGGER.warn("#migrated :" + this.productSku);
    } catch (Exception e) {
      result = this.productSku + "#failure cause:" + e.getMessage();
      MigrationTask.LOGGER.warn("#failed-sku:{}", this.productSku);
      MigrationTask.LOGGER.error(result, e);
      if (e.getMessage().contains("E11000 duplicate key ")) {
        MigrationTask.LOGGER
            .warn("#not-migrated:{} with error duplicated product", this.productSku);
      } else if (e.getMessage().contains("Index: 0, Size: 0")) {
        MigrationTask.LOGGER.warn("with error doesn't have catentdesc record#not-migrated:{}",
            this.productSku);
      } else {
        MigrationTask.LOGGER
            .warn("#not-migrated:{} with error {}", this.productSku, e.getMessage());
      }
    }
    MigrationTask.LOGGER.info("end migrating {} at {}", this.productSku, new Date());
    MigrationTask.LOGGER.info("call : {}", stopwatch.elapsed(TimeUnit.NANOSECONDS) - elapsedAll);
    return result;
  }


  public void setProductDb2Repository(ProductDb2Repository productDb2Repository) {
    this.productDb2Repository = productDb2Repository;
  }

  public void setProductSku(String productSku) {
    this.productSku = productSku;
  }

}
