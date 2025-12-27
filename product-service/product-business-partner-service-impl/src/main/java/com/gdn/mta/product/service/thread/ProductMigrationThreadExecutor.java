package com.gdn.mta.product.service.thread;

import java.util.List;
import java.util.concurrent.Callable;

import org.apache.commons.collections4.CollectionUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.slf4j.MDC;

import com.gdn.mta.product.util.GdnMandatoryRequestParameterUtil;
import com.gdn.mta.product.entity.ProductMigration;
import com.gdn.mta.product.service.ProductMigrationWrapperService;
import com.gdn.partners.pbp.commons.constants.Constants;

import lombok.extern.slf4j.Slf4j;

@Slf4j
public class ProductMigrationThreadExecutor implements Callable<Integer> {

  private List<String> productList;
  private List<String> productSkuList;
  private ProductMigrationWrapperService productMigrationWrapperService;
  private static final Logger LOGGER = LoggerFactory.getLogger(ProductMigrationThreadExecutor.class);

  public ProductMigrationThreadExecutor(ProductMigrationWrapperService productMigrationWrapperService,
      List<String> productList, List<String> productSkuList) {
    this.productList = productList;
    this.productMigrationWrapperService = productMigrationWrapperService;
    this.productSkuList = productSkuList;
  }

  private ProductMigrationWrapperService getProductMigrationWrapperService() {
    return productMigrationWrapperService;
  }

  @Override
  public Integer call() throws Exception {
    setMandatoryParameters();
    LOGGER
        .info("Product Migration : Thread {} product size : {} productSKu size : {}", Thread.currentThread().getName(),
            productList.size(), productSkuList.size());
    int migrationCount = 0;
    if (CollectionUtils.isNotEmpty(productList)) {
      for (String product : productList) {
        migrationCount = migrationCount + getProductMigrationWrapperService().migrateProductByProductCode(product, true);
      }
    }
    if (CollectionUtils.isNotEmpty(productSkuList)) {
      migrationCount = migrationCount + getProductMigrationWrapperService().migrateProductByProductSkus(productSkuList);
    }
    return migrationCount;
  }

  private void setMandatoryParameters() {
    MDC.put(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER, Constants.DEFAULT_STORE_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, Constants.DEFAULT_REQUEST_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, Constants.DEFAULT_USERNAME);
    MDC.put(GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER, Constants.DEFAULT_CLIENT_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER, Constants.DEFAULT_CHANNEL_ID);
  }
}
