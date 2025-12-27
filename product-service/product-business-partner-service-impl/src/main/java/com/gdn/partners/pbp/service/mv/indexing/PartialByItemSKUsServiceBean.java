package com.gdn.partners.pbp.service.mv.indexing;

import java.util.List;
import java.util.Map;

import org.apache.commons.collections4.MapUtils;
import org.apache.commons.lang3.time.StopWatch;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;

import com.gdn.common.web.param.MandatoryRequestParam;
import com.gdn.partners.pbp.service.productlevel3.ProductLevel3AggregatorService;
import com.gdn.partners.pbp.util.MandatoryParameterUtil;
import com.gdn.partners.pbp.util.ProductAggregatorIndexingUtil;
import com.newrelic.api.agent.Trace;

@Service(value = PartialByItemSKUsServiceBean.BEAN_NAME + IndexingService.SUFFIX_BEAN_NAME)
public class PartialByItemSKUsServiceBean implements IndexingService {

  public static final String BEAN_NAME = "PartialByItemSKUs";

  @Autowired
  private ProductLevel3AggregatorService productLevel3AggregatorService;
  
  @Autowired
  private ProductAggregatorIndexingUtil productAggregatorIndexingUtil;

  private static final Logger LOG = LoggerFactory.getLogger(PartialByItemSKUsServiceBean.class);

  @Trace(dispatcher=true)
  @Override
  @Async("merchantProductMVIndexingExecutor")
  public void doIndexing(MandatoryRequestParam mandatoryRequestParam, Map<String, Object> parameter,
      boolean isForce) throws Exception {
    MandatoryParameterUtil.mandatoryParameterSetter(mandatoryRequestParam);
    if (isForce || !this.productAggregatorIndexingUtil.isCurrentIndexingRunning()) {
      upsertProductAggregator(mandatoryRequestParam, parameter);
    } else {
      LOG.warn(
          "LogId: {}. cannot executing PartialIndexingService by item skus, indexing is still running",
          mandatoryRequestParam.getStoreId());
    }
  }

  @SuppressWarnings("unchecked")
  private void upsertProductAggregator(MandatoryRequestParam mandatoryRequestParam,
      Map<String, Object> itemSKUs) {
    StopWatch stopWatch = new StopWatch();
    stopWatch.start();
    this.productAggregatorIndexingUtil.setIndexingRunning();
    if (MapUtils.isNotEmpty(itemSKUs)) {
      for (String businessPartnerCode : itemSKUs.keySet()) {
        for (String itemSKU : (List<String>) itemSKUs.get(businessPartnerCode)) {
          try {
            this.productLevel3AggregatorService.update(itemSKU, businessPartnerCode);
          } catch (Exception e) {
            LOG.error(
                "LogId: {}. Error when update to DB partial indexing merchant product aggregator by itemSku. businessPartnerCode: {}, itemSku: {}",
                mandatoryRequestParam.getStoreId(), businessPartnerCode, itemSKU, e);
          }
        }
      }
    }
    this.productAggregatorIndexingUtil.delIndexingRunningProcess();
    stopWatch.stop();
    LOG.info(
        "LogId: {}. Finished running partial indexing merchant product aggregator by itemSku. running time : {} ms",
        mandatoryRequestParam.getStoreId(), stopWatch.getTime());

  }

}
