package com.gdn.mta.product.service.domainevent;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;

import org.apache.commons.collections.CollectionUtils;
import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.product.util.GdnMandatoryRequestParameterUtil;
import com.gdn.mta.domain.event.config.DomainEventName;
import com.gdn.mta.domain.event.modal.ProductMigrationRequestEvent;
import com.gdn.mta.product.service.ProductMigrationWrapperService;
import com.gdn.mta.product.service.ProductSystemParameterService;
import com.gdn.mta.product.service.thread.ProductMigrationThreadExecutor;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.newrelic.api.agent.Trace;

import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
public class ProductMigrationSubscriber {

  @Autowired
  private ProductMigrationWrapperService productMigrationWrapperService;

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private ProductSystemParameterService productSystemParameterService;

  @Trace(dispatcher=true)
  @KafkaListener(topics = DomainEventName.MIGRATE_PRODUCTS_EVENT, autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) throws Exception {
    try {
      //adding switch in case the we need to stop the migration of products
      boolean productMigrationSwitch = Boolean.valueOf(productSystemParameterService
          .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID,
              com.gdn.partners.pbp.commons.constants.Constants.PRODUCT_MIGRATION_SWITCH).getValue());
      if (productMigrationSwitch) {
        ProductMigrationRequestEvent requestEvent = objectMapper.readValue(message, ProductMigrationRequestEvent.class);
        log.info("Listening to event : {} with payload : {}", DomainEventName.MIGRATE_PRODUCTS_EVENT, requestEvent);
        List<String> productCodes = requestEvent.getProductCodes();
        List<String> productSkus = requestEvent.getProductSkuList();
        int threads = requestEvent.getThreadCount();

        if (threads > 1) {
          //executor service
          ExecutorService executorService = Executors.newFixedThreadPool(threads);
          int minProductsPerThread = getListSize(productCodes, productSkus) / threads;
          int maxProductPerThread = minProductsPerThread + 1;
          int threadsWithMaxProducts = getListSize(productCodes, productSkus) - threads * minProductsPerThread;
          int start = 0;
          List<Future<Integer>> futures = new ArrayList<>(getListSize(productCodes, productSkus));
          for (int i = 0; i < threads; i++) {
            int productCount = (i < threadsWithMaxProducts ? maxProductPerThread : minProductsPerThread);
            int end = start + productCount;
            Callable<Integer> callable;
            if (CollectionUtils.isNotEmpty(productCodes)) {
              callable =
                  new ProductMigrationThreadExecutor(productMigrationWrapperService, productCodes.subList(start, end),
                      new ArrayList<>());
            } else {
              callable = new ProductMigrationThreadExecutor(productMigrationWrapperService, new ArrayList<>(),
                  productSkus.subList(start, end));
            }
            Future<Integer> future = executorService.submit(callable);
            futures.add(future);
            start = end;
          }
          for (Future<Integer> future : futures) {
            future.get();
          }
        } else {
          migrateProducts(requestEvent, productCodes, productSkus);
        }
      }
    } catch (Exception e) {
      log.error("Error while listening to event : {} with payload : {} ", DomainEventName.MIGRATE_PRODUCTS_EVENT,
          message, e);
    }
  }

  private void migrateProducts(ProductMigrationRequestEvent requestEvent, List<String> productCodes,
      List<String> productSkus) throws Exception {
    setMandatoryParameters();
    log.info("Starting migration for event : {} with payload : {}", DomainEventName.MIGRATE_PRODUCTS_EVENT,
        requestEvent);
    int migrationCount = 0;
    if (CollectionUtils.isNotEmpty(productCodes)) {
      for (String product : productCodes) {
        migrationCount =
            migrationCount + productMigrationWrapperService.migrateProductByProductCode(product, true);
      }
    }
    if (CollectionUtils.isNotEmpty(productSkus)) {
      migrationCount = migrationCount + productMigrationWrapperService.migrateProductByProductSkus(productSkus);
    }
    log.info("Completed migration for {} products for event : {} with payload : {}", migrationCount,
        DomainEventName.MIGRATE_PRODUCTS_EVENT, requestEvent);
  }

  private Integer getListSize(List<String> productCodes, List<String> productMigrationList) {
    if (CollectionUtils.isNotEmpty(productCodes)) {
      return productCodes.size();
    }
    if (CollectionUtils.isNotEmpty(productMigrationList)) {
      return productMigrationList.size();
    }
    return 0;
  }

  private void setMandatoryParameters() {
    MDC.put(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER, Constants.DEFAULT_STORE_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, Constants.DEFAULT_REQUEST_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, Constants.DEFAULT_USERNAME);
    MDC.put(GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER, Constants.DEFAULT_CLIENT_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER, Constants.DEFAULT_CHANNEL_ID);
  }
}
