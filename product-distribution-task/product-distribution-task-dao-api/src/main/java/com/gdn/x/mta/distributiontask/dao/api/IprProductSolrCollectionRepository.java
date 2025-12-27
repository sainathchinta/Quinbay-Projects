package com.gdn.x.mta.distributiontask.dao.api;

import com.gdn.x.mta.distributiontask.model.cache.CacheKeys;
import com.gdn.x.mta.distributiontask.model.dto.IPRProductListRequest;
import com.gdn.x.mta.distributiontask.model.solr.IPRProductSolr;
import org.apache.solr.client.solrj.SolrServerException;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

import java.io.IOException;
import java.util.Map;


public interface IprProductSolrCollectionRepository {

  /**
   * @param storeId               String
   * @param iprProductListRequest IPRProductListRequest
   * @param pageable              Pageable
   * @return Page<IPRProductSolr>
   * @throws Exception Exception
   */
  Page<IPRProductSolr> getIprProductsList(String storeId,
    IPRProductListRequest iprProductListRequest, Pageable pageable) throws Exception;

  /**
   * to add product to ipr solr
   *
   * @param iprProductSolr IPRProductSolr
   */
  void addDocumentToIPRSolr(IPRProductSolr iprProductSolr);

  /**
   * to delete ipr solr document
   *
   * @param productSku String
   */
  void deleteIprSolrDocument(String productSku);

  @Cacheable(value = CacheKeys.IPR_PRODUCT_COUNT, key = "#storeId", unless = "#result == null")
  Map<String, Object> getPrimaryFilterCounts(String storeId)
    throws IOException, SolrServerException;
}
