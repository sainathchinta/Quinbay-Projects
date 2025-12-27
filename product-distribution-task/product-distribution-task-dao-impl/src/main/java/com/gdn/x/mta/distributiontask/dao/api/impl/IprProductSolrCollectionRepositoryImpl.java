package com.gdn.x.mta.distributiontask.dao.api.impl;

import com.gdn.x.mta.distributiontask.dao.api.IprProductSolrCollectionRepository;
import com.gdn.x.mta.distributiontask.dao.util.ConverterUtil;
import com.gdn.x.mta.distributiontask.dao.util.IPRProductSolrHelper;
import com.gdn.x.mta.distributiontask.dao.util.VendorProductSolrHelper;
import com.gdn.x.mta.distributiontask.model.cache.CacheKeys;
import com.gdn.x.mta.distributiontask.model.dto.IPRProductListRequest;
import com.gdn.x.mta.distributiontask.model.solr.IPRProductSolr;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.apache.solr.client.solrj.SolrQuery;
import org.apache.solr.client.solrj.SolrServerException;
import org.apache.solr.client.solrj.impl.CloudSolrClient;
import org.apache.solr.client.solrj.response.QueryResponse;
import org.apache.solr.common.SolrInputDocument;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Repository;

import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

@Slf4j
@Repository
public class IprProductSolrCollectionRepositoryImpl implements IprProductSolrCollectionRepository {


  @Autowired
  @Qualifier(value = "iprProductCollectionClient")
  private CloudSolrClient iprProductCollectionClient;

  @Override
  public Page<IPRProductSolr> getIprProductsList(String storeId,
    IPRProductListRequest iprProductListRequest, Pageable pageable) throws Exception {
    List<IPRProductSolr> solrList = new ArrayList<>();
    SolrQuery solrQuery =
      VendorProductSolrHelper.getSolrQueryForIprProductList(storeId, iprProductListRequest,
        pageable);
    log.info("#getIprProductsListBySolrQuery : solrQuery was : {} ", solrQuery);
    QueryResponse queryResponse = this.iprProductCollectionClient.query(solrQuery);
    long totalRecords = queryResponse.getResults().getNumFound();
    queryResponse.getResults().forEach(solrDocument -> {
      IPRProductSolr iprProductSolr =
        VendorProductSolrHelper.getSolrIprProductCollection(solrDocument);
      solrList.add(iprProductSolr);
    });
    return new PageImpl<>(solrList, pageable, totalRecords);
  }

  @Override
  public void addDocumentToIPRSolr(IPRProductSolr iprProductSolr) {
    try {
      if (Objects.nonNull(iprProductSolr)) {
        SolrInputDocument solrInputDocument =
            IPRProductSolrHelper.toIPRrSolrInputDocument(iprProductSolr);
        iprProductCollectionClient.add(solrInputDocument);
      }
    } catch (Exception e) {
      log.error("Error while adding document to IPR solr : {} ", iprProductSolr.getProductSku(), e);
    }
  }

  @Override
  public void deleteIprSolrDocument(String productSku) {
    try {
      if (StringUtils.isNotBlank(productSku)) {
        log.info("deleting the product: {} from ipr solr.", productSku);
        iprProductCollectionClient.deleteByQuery(
            VendorProductSolrHelper.getIprQueryForDelete(productSku));
      }
    } catch (Exception e) {
      log.error("Error occurred while deleting document from IPR solr for product : {} ",
          productSku, e);
    }
  }

  @Override
  @Cacheable(value = CacheKeys.IPR_PRODUCT_COUNT, key = "#storeId", unless = "#result == null")
  public Map<String, Object> getPrimaryFilterCounts(String storeId)
    throws IOException, SolrServerException {
    SolrQuery solrQuery = IPRProductSolrHelper.getSolrQueryForPrimaryFilterCounts(storeId);
    log.info("#getPrimaryFilterCounts : solrQuery was : {} ", solrQuery);
    QueryResponse filterCounts = this.iprProductCollectionClient.query(solrQuery);
    Map<String, Object> countResponse = new HashMap<>();
    if (Objects.nonNull(filterCounts)) {
      ConverterUtil.processIntervalFacets(countResponse, filterCounts);
      ConverterUtil.processStateFacetFieldsForIpr(countResponse, filterCounts);
      ConverterUtil.extractAssignedAndUnassignedCounts(countResponse,filterCounts);
    }
    return countResponse;
  }
}
