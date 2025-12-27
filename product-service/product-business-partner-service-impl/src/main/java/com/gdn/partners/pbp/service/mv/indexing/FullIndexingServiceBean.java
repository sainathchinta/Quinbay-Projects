package com.gdn.partners.pbp.service.mv.indexing;

import java.util.List;
import java.util.Map;

import org.apache.commons.lang3.time.StopWatch;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;

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

@Service(value = FullIndexingServiceBean.BEAN_NAME + IndexingService.SUFFIX_BEAN_NAME)
public class FullIndexingServiceBean implements IndexingService {

  public static final String BEAN_NAME = "FullIndexing";

  private static final String ORDER_BY_CREATED_DATE = "createdDate";

  private static final String SORT_ASC = "asc";

  private static final Logger LOG = LoggerFactory.getLogger(FullIndexingServiceBean.class);

  @Autowired
  private ProductLevel3Repository productLevel3Repository;

  @Autowired
  private ProductLevel3AggregatorService productLevel3AggregatorService;

  @Autowired
  private ApplicationProperties applicationProperties;

  @Autowired
  private ProductAggregatorIndexingUtil productAggregatorIndexingUtil;

  @Trace(dispatcher=true)
  @Override
  @Async("merchantProductMVIndexingExecutor")
  public void doIndexing(MandatoryRequestParam mandatoryRequestParam, Map<String, Object> parameter, boolean isForce)
      throws Exception {
    MandatoryParameterUtil.mandatoryParameterSetter(mandatoryRequestParam);
    if (isForce || !this.productAggregatorIndexingUtil.isCurrentIndexingRunning()) {
      this.populateProduct(mandatoryRequestParam);
    } else {
      LOG.warn("LogId: {}. cannot executing FullIndexingService, indexing is still running",
          mandatoryRequestParam.getStoreId());
    }
  }

  private void populateProduct(MandatoryRequestParam mandatoryRequestParam) throws Exception {
    int currentPage = 0;
    int totalPage = 0;
    StopWatch stopWatch = new StopWatch();
    stopWatch.start();
    this.productAggregatorIndexingUtil.setIndexingRunning();
    do {
      try {
        ItemSummaryRequest itemSummaryRequest = new ItemSummaryRequest();
        Page<ItemSummaryResponse> productGdnSKUPages = productLevel3Repository.findSummaryByFilter(itemSummaryRequest,
            PageRequest.of(currentPage, this.applicationProperties.getBatchFullIndexingSize()),
            new SortOrder(ORDER_BY_CREATED_DATE, SORT_ASC));
        totalPage = productGdnSKUPages.getTotalPages();
        this.upsertProductAggregator(mandatoryRequestParam, productGdnSKUPages.getContent());
      } catch (Exception e) {
        LOG.error(
            "LogId: {}. Error when partial indexing merchant product aggregator by businessPartnerCode. businessPartnerCode: {}",
            mandatoryRequestParam.getStoreId(), e);
      }
      currentPage++;
    } while (currentPage < totalPage);
    this.productAggregatorIndexingUtil.delIndexingRunningProcess();
    stopWatch.stop();
    LOG.info("LogId: {}. Finished running full indexing merchant product aggregator. running time : {} ms",
        mandatoryRequestParam.getStoreId(), stopWatch.getTime());
  }

  private void upsertProductAggregator(MandatoryRequestParam mandatoryRequestParam,
      List<ItemSummaryResponse> productList) {
    for (ItemSummaryResponse product : productList) {
      try {
        this.productLevel3AggregatorService.update(product.getItemSku(), product.getMerchantCode());
      } catch (Exception e) {
        LOG.error("LogId: {}. Error when update to DB full indexing merchant product aggregator. itemSku: {}",
            mandatoryRequestParam.getStoreId(), product.getItemSku(), e);
      }
    }
  }
}
