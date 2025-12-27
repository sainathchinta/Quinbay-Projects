package com.gdn.mta.product.service.solr;

import java.text.SimpleDateFormat;
import java.util.Arrays;
import java.util.Date;
import java.util.List;

import org.apache.commons.lang3.StringUtils;
import org.apache.solr.client.solrj.SolrClient;
import org.apache.solr.client.solrj.impl.CloudSolrClient;
import org.apache.solr.client.solrj.impl.HttpSolrClient;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;


import com.gdn.mta.product.entity.ProductCollection;
import com.gdn.mta.product.entity.ProductSystemParameter;
import com.gdn.mta.product.entity.UpdatedProductHistory;
import com.gdn.mta.product.repository.ProductCollectionRepository;
import com.gdn.mta.product.repository.UpdatedProductHistoryRepository;
import com.gdn.mta.product.repository.SolrHistoryCollectionRepository;
import com.gdn.mta.product.repository.SolrIndexingRepository;
import com.gdn.mta.product.service.ProductService;
import com.gdn.mta.product.service.ProductSystemParameterService;
import com.gdn.mta.product.util.SummaryFilterUtil;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.partners.pbp.commons.constants.SystemParameterConstants;
import com.newrelic.api.agent.Trace;

import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
public class SolrIndexingServiceBean implements SolrIndexingService {

  @Autowired
  private SolrIndexingRepository solrIndexingRepository;

  @Value("${solr.server.url}")
  private String solrConnectionUrl;

  @Value("${solr.cloud.urls}")
  private String solrCloudUrls;

  @Autowired
  private ProductSystemParameterService systemParameterService;

  @Autowired
  private UpdatedProductHistoryRepository updatedProductHistoryRepository;

  @Autowired
  private SolrHistoryCollectionRepository solrHistoryCollectionRepository;

  @Autowired
  private ProductCollectionRepository productCollectionRepository;

  @Autowired
  private ProductService productService;

  private static final String COMMA_SEPARATOR = ",";
  private static final String SLASH = "/";

  @Trace(dispatcher=true)
  @Override
  @Async
  public void updateAll(String sourceCollection, String destinationCollection) throws Exception {
    try {
      SolrClient sourceSolrClient = new HttpSolrClient.Builder(solrConnectionUrl + SLASH + sourceCollection).build();
      CloudSolrClient destinationSolrClient =
          new CloudSolrClient.Builder().withZkHost(Arrays.asList(solrCloudUrls.split(COMMA_SEPARATOR))).build();
      destinationSolrClient.setDefaultCollection(destinationCollection);
      solrIndexingRepository.updateAll(sourceSolrClient, destinationSolrClient);
    } catch (Exception e) {
      log.error("Exception caught while updating documents to solr ", e);
    }
  }

  @Override
  @Async
  public void reindexProductHistoryByProductSkus(String storeId, List<String> productSkus) {
    int pageSize = Integer.valueOf(
        systemParameterService.findByStoreIdAndVariable(storeId, SystemParameterConstants.HISTORY_REINDEXING_PAGE_SIZE)
            .getValue());
    for (String productSku : productSkus) {
      int page = 0;
      long totalRecords = 0;
      do {
        Page<UpdatedProductHistory> logAuditTrailUpdatedProducts = updatedProductHistoryRepository
            .findByProductSkuOrderByAccessTimeDesc(productSku, PageRequest.of(page, pageSize));
        if (!logAuditTrailUpdatedProducts.getContent().isEmpty()) {
          solrHistoryCollectionRepository
              .addDocument(SummaryFilterUtil.toSolrInputDocumentsForHistory(logAuditTrailUpdatedProducts.getContent()));
        }
        totalRecords = logAuditTrailUpdatedProducts.getTotalElements();
        page++;
      } while (page * pageSize < totalRecords);
    }
  }

  @Override
  @Async
  public void deltaReindexHistoryCollection(String storeId, String startTime, String endTime) {
    SimpleDateFormat simpleDateFormat = new SimpleDateFormat(Constants.DATE_PATTERN);
    try {
      Date startDate = simpleDateFormat.parse(startTime);
      Date endDate;
      if (StringUtils.isNotEmpty(endTime)) {
        endDate = simpleDateFormat.parse(endTime);
      } else {
        endDate = new Date();
      }
      int pageSize = Integer.valueOf(systemParameterService
          .findByStoreIdAndVariable(storeId, SystemParameterConstants.HISTORY_REINDEXING_PAGE_SIZE).getValue());
      int page = 0;
      long totalRecords = 0;
      int totalDocsReindexed = 0;
      do {
        Page<UpdatedProductHistory> logAuditTrailUpdatedProducts = updatedProductHistoryRepository
            .findByAccessTimeBetweenOrderByAccessTime(startDate, endDate,
                PageRequest.of(page, pageSize));
        if (!logAuditTrailUpdatedProducts.getContent().isEmpty()) {
          totalDocsReindexed += logAuditTrailUpdatedProducts.getContent().size();
          solrHistoryCollectionRepository
              .addDocument(SummaryFilterUtil.toSolrInputDocumentsForHistory(logAuditTrailUpdatedProducts.getContent()));
        }
        totalRecords = logAuditTrailUpdatedProducts.getTotalElements();
        page++;
      } while (page * pageSize < totalRecords);
      log.info("Total documents re-indexed: {} ", totalDocsReindexed);
    } catch (Exception e) {
      log.error("Error while delta re-indexing. startTime: {}, endTime: {} ", startTime, endTime, e);
    }
  }

  @Override
  @Async
  public void deltaReindexPrdProductCollection(String storeId, String indexFrom, String indexTill) {
    ProductSystemParameter systemParameter = systemParameterService.findByStoreIdAndVariable(storeId,
        SystemParameterConstants.LAST_DELTA_REINDEX_TIME_FOR_PRD_PRODUCT_COLLECTION);
    Date indexTillDate = new Date();
    try {
      Date indexFromDate = StringUtils.isEmpty(indexFrom) ?
          new Date(Long.parseLong(systemParameter.getValue())) :
          new SimpleDateFormat(com.gdn.x.product.enums.Constants.DATE_FORMAT).parse(indexFrom);
      indexTillDate = StringUtils.isEmpty(indexTill) ?
          new Date() :
          new SimpleDateFormat(com.gdn.x.product.enums.Constants.DATE_FORMAT).parse(indexTill);
      List<ProductCollection> productCollectionList =
          productCollectionRepository.findByMarkForDeleteFalseAndViewableTrueAndActivatedTrueAndUpdatedDateBetweenOrderByUpdatedDateAsc(
              indexFromDate, indexTillDate);
      log.info("delta reindex for prd product collection from Date : {} to {}", indexFromDate, indexTillDate);
      for (ProductCollection productCollection : productCollectionList) {
        productService.updateSolrProductCollection(productCollection);
      }
    } catch (Exception e) {
      log.error("Error while delta re-indexing. startTime: {}, endTime: {} ", indexFrom, indexTill, e);
    } finally {
      systemParameter.setValue(String.valueOf(indexTillDate.getTime()));
      systemParameter.setUpdatedBy(com.gdn.x.product.enums.Constants.DEFAULT_UPDATED_BY);
      systemParameterService.update(systemParameter);
    }
  }

}

