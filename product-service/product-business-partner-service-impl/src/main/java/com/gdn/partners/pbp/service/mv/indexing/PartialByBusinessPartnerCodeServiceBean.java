package com.gdn.partners.pbp.service.mv.indexing;

import java.util.List;
import java.util.Map;

import org.apache.commons.lang3.time.StopWatch;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.scheduling.annotation.Async;
import org.springframework.scheduling.concurrent.ThreadPoolTaskExecutor;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

import com.gdn.common.web.param.MandatoryRequestParam;
import com.gdn.mta.product.config.ApplicationProperties;
import com.gdn.mta.product.repository.ProductLevel3Repository;
import com.gdn.mta.product.valueobject.SortOrder;
import com.gdn.partners.pbp.service.productlevel3.ProductLevel3AggregatorService;
import com.gdn.partners.pbp.util.MandatoryParameterUtil;
import com.gdn.partners.pbp.util.ProductAggregatorIndexingUtil;
import com.gdn.x.product.rest.web.model.dto.ItemSummaryResponse;
import com.gdn.x.product.rest.web.model.request.ItemSummaryRequest;
import com.newrelic.api.agent.Trace;

@Service(value = PartialByBusinessPartnerCodeServiceBean.BEAN_NAME + IndexingService.SUFFIX_BEAN_NAME)
public class PartialByBusinessPartnerCodeServiceBean implements IndexingService {

  public static final String BEAN_NAME = "PartialByBusinessPartnerCode";
  public static final String BUSINESS_PARTNER_CODES = "businessPartnerCodes";
  private static final String ORDER_BY_CREATED_DATE = "createdDate";
  private static final String SORT_ASC = "asc";
  private static final Logger LOGGER = LoggerFactory.getLogger(PartialByBusinessPartnerCodeServiceBean.class);
  private static Object LOCK = new Object();

  @Autowired
  private ProductLevel3Repository productLevel3Repository;

  @Autowired
  private ProductLevel3AggregatorService productLevel3AggregatorService;

  @Autowired
  private ApplicationProperties applicationProperties;

  @Autowired
  private ProductAggregatorIndexingUtil productAggregatorIndexingUtil;

  @Autowired
  @Qualifier(value = "productLevel3AggregatorPartialIndexerTaskExecutor")
  private ThreadPoolTaskExecutor productLevel3AggregatorPartialIndexerTaskExecutor;

  @Trace(dispatcher=true)
  @Override
  @Async("merchantProductMVIndexingExecutor")
  public void doIndexing(MandatoryRequestParam mandatoryRequestParam, Map<String, Object> parameter, boolean isForce)
      throws Exception {
    MandatoryParameterUtil.mandatoryParameterSetter(mandatoryRequestParam);
    if (isForce || !this.productAggregatorIndexingUtil.isCurrentIndexingRunning()) {
      this.productAggregatorIndexingUtil.setIndexingRunning();
      List<String> businessPartnerCodes =
          (List<String>) parameter.get(PartialByBusinessPartnerCodeServiceBean.BUSINESS_PARTNER_CODES);
      if (!CollectionUtils.isEmpty(businessPartnerCodes)) {
        for (String businessPartnerCode : businessPartnerCodes) {
          while (true) {
            if (this.productLevel3AggregatorPartialIndexerTaskExecutor.getActiveCount() < this.productLevel3AggregatorPartialIndexerTaskExecutor
                .getCorePoolSize()) {
              break;
            } else {
              synchronized (PartialByBusinessPartnerCodeServiceBean.LOCK) {
                PartialByBusinessPartnerCodeServiceBean.LOCK.wait();
              }
            }
          }
          PartialByBusinessPartnerCodeServiceBean.LOGGER.info(
              "LogId: Executing PartialIndexing process by businessPartnerCode: {}", businessPartnerCode);
          this.productLevel3AggregatorPartialIndexerTaskExecutor.execute(new ProductLevel3AggregatorPartialIndexer(
              this.applicationProperties, this.productLevel3AggregatorService, mandatoryRequestParam,
              businessPartnerCode));
        }
      }
      this.productAggregatorIndexingUtil.delIndexingRunningProcess();
    } else {
      LOGGER
          .warn(
              "LogId: {}. cannot executing PartialIndexingService by businessPartnerCode, indexing is still running. businessPartnerCode: {}",
              mandatoryRequestParam.getStoreId(), parameter.get(BUSINESS_PARTNER_CODES));
    }
  }

  public class ProductLevel3AggregatorPartialIndexer implements Runnable {

    private ApplicationProperties applicationProperties;
    private ProductLevel3AggregatorService productLevel3AggregatorService;
    private MandatoryRequestParam mandatoryRequestParam;
    private String businessPartnerCode;

    public ProductLevel3AggregatorPartialIndexer(ApplicationProperties applicationProperties,
        ProductLevel3AggregatorService productLevel3AggregatorService, MandatoryRequestParam mandatoryRequestParam,
        String businessPartnerCode) {
      super();
      this.applicationProperties = applicationProperties;
      this.productLevel3AggregatorService = productLevel3AggregatorService;
      this.mandatoryRequestParam = mandatoryRequestParam;
      this.businessPartnerCode = businessPartnerCode;
    }

    @Override
    public void run() {
      this.populateProductAndStockByBusinessPartnerCode(this.mandatoryRequestParam, this.businessPartnerCode);
      synchronized (PartialByBusinessPartnerCodeServiceBean.LOCK) {
        PartialByBusinessPartnerCodeServiceBean.LOCK.notify();
      }
    }

    private void populateProductAndStockByBusinessPartnerCode(MandatoryRequestParam mandatoryRequestParam,
        String businessPartnerCode) {
      int currentPage = 0;
      int totalPage = 0;
      StopWatch stopWatch = new StopWatch();
      stopWatch.start();
      do {
        try {
          ItemSummaryRequest itemSummaryRequest = new ItemSummaryRequest();
          itemSummaryRequest.setMerchantCode(businessPartnerCode);
          Page<ItemSummaryResponse> productGdnSKUPages =
              productLevel3Repository.findSummaryByFilter(itemSummaryRequest, PageRequest.of(currentPage,
                  this.applicationProperties.getBatchPartialIndexingSize()), new SortOrder(ORDER_BY_CREATED_DATE,
                  SORT_ASC));
          totalPage = productGdnSKUPages.getTotalPages();
          this.upsertProductAggregator(mandatoryRequestParam, productGdnSKUPages.getContent(), businessPartnerCode);
        } catch (Exception e) {
          LOGGER
              .error(
                  "LogId: {}. Error when partial indexing merchant product aggregator by businessPartnerCode. businessPartnerCode: {}",
                  mandatoryRequestParam.getStoreId(),businessPartnerCode, e);
        }
        currentPage++;
      } while (currentPage < totalPage);
      stopWatch.stop();
      LOGGER
          .info(
              "LogId: {}. Finished running partial indexing merchant product aggregator by businessPartnerCode. businessPartnerCode: {}, running time : {} ms",
              mandatoryRequestParam.getStoreId(), businessPartnerCode, stopWatch.getTime());
    }


    private void upsertProductAggregator(MandatoryRequestParam mandatoryRequestParam,
        List<ItemSummaryResponse> productList, String businessPartnerCode) {
      for (ItemSummaryResponse product : productList) {
        try {
          this.productLevel3AggregatorService.update(product.getItemSku(), businessPartnerCode);
        } catch (Exception e) {
          LOGGER
              .error(
                  "LogId: {}. Error when update to DB partial indexing merchant product aggregator by businessPartnerCode. businessPartnerCode: {}, itemSku: {}",
                  mandatoryRequestParam.getStoreId(), businessPartnerCode, product.getItemSku(), e);
        }
      }
    }

  }

}
