package com.gdn.x.product.dao.solr.impl;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;

import com.gdn.x.product.model.vo.ProductAndItemsVO;
import com.gdn.x.product.outbound.api.InventoryOutbound;
import com.gdn.x.product.service.api.ItemService;
import org.apache.commons.collections.CollectionUtils;
import org.apache.solr.client.solrj.SolrClient;
import org.apache.solr.client.solrj.SolrQuery;
import org.apache.solr.client.solrj.SolrServerException;
import org.apache.solr.client.solrj.response.QueryResponse;
import org.apache.solr.client.solrj.response.UpdateResponse;
import org.apache.solr.common.SolrDocumentList;
import org.apache.solr.common.SolrInputDocument;
import org.apache.solr.common.params.CommonParams;
import org.apache.solr.common.params.CursorMarkParams;
import org.apache.solr.common.util.SimpleOrderedMap;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Component;

import com.gdn.x.product.dao.solr.api.SolrIndexingRepository;
import com.gdn.x.product.enums.SolrConstants;
import com.gdn.x.product.enums.SolrFieldNames;
import com.gdn.x.product.service.util.CommonUtil;

import lombok.extern.slf4j.Slf4j;

@Component
@Slf4j
public class SolrIndexingRepositoryBean implements SolrIndexingRepository {

  private static final String ID = "id";
  private static final String SOLR_UPDATE_ERROR_MESSAGE = "Error while updating the Documents to SOLR,";
  private static final String BATCHED_UPDATE_ERROR_MESSAGE =
      "Error occurred while updating documents batch number :{} ";
  private static final String QUERY = "id:*";
  private static final String DELIMETER = "#_#";

  @Value("${solr.indexing.batch.size:10000}")
  private int batchSize;

  @Autowired
  @Lazy
  private ItemService itemService;

  @Autowired
  private InventoryOutbound inventoryOutbound;

  @Override
  public void updateAll(SolrClient sourceCollection, SolrClient destinationCollection) throws Exception {
    SolrQuery solrQuery = new SolrQuery(QUERY);
    solrQuery.setSort(SolrQuery.SortClause.asc(ID));
    solrQuery.setFields(getFields(sourceCollection));
    solrQuery.setRows(batchSize);
    int batchNo = 1;
    String cursor = CursorMarkParams.CURSOR_MARK_START;
    boolean isLastBatch = false;
    QueryResponse queryResponse = null;
    while (!isLastBatch) {
      try {
        solrQuery.set(CursorMarkParams.CURSOR_MARK_PARAM, cursor);
        queryResponse = sourceCollection.query(solrQuery);
        if (Objects.nonNull(queryResponse) && CollectionUtils.isNotEmpty(queryResponse.getResults())) {
          update(convertToInputDoc(queryResponse.getResults(), null), destinationCollection);
        }
        isLastBatch = queryResponse.getNextCursorMark().equals(cursor);
        cursor = queryResponse.getNextCursorMark();
        log.info(cursor);
      } catch (Exception ex) {
        log.error(BATCHED_UPDATE_ERROR_MESSAGE, batchNo, ex);
        isLastBatch = queryResponse.getNextCursorMark().equals(cursor);
        cursor = queryResponse.getNextCursorMark();
      }
      log.debug("Done {}", batchNo++ * batchSize);
    }
    destinationCollection.commit();
  }

  @Override
  public void copyProductsToL3Collection(String storeId, SolrClient sourceCollection, SolrClient destinationCollection,
      List<String> categoryCodes, String sortOrder, int reindexBatchSize, int inventoryBatchSize) throws Exception {
    SolrQuery solrQuery = getSolrQuery(categoryCodes, sortOrder, reindexBatchSize);
    int batchNo = 1;
    String cursor = CursorMarkParams.CURSOR_MARK_START;
    boolean isLastBatch = false;
    QueryResponse queryResponse = new QueryResponse();
    while (!isLastBatch) {
      try {
        solrQuery.set(CursorMarkParams.CURSOR_MARK_PARAM, cursor);
        queryResponse = sourceCollection.query(solrQuery);
        List<String> productSkus = queryResponse.getResults().stream()
            .map(solrDocument -> (String) solrDocument.getFieldValue(SolrFieldNames.PRODUCT_SKU))
            .collect(Collectors.toList());
        Map<String, ProductAndItemsVO> productSkuAndProductAndItemsVOMap =
            this.itemService.getProductAndItemsMap(storeId, productSkus);
        if (Objects.nonNull(queryResponse) && CollectionUtils.isNotEmpty(queryResponse.getResults())) {
          update(
              convertToL3SolrDocument(queryResponse.getResults(), productSkuAndProductAndItemsVOMap), destinationCollection);
        }
        isLastBatch = queryResponse.getNextCursorMark().equals(cursor);
        cursor = queryResponse.getNextCursorMark();
        log.debug(cursor);
      } catch (Exception ex) {
        log.error(BATCHED_UPDATE_ERROR_MESSAGE, batchNo, ex);
        isLastBatch = queryResponse.getNextCursorMark().equals(cursor);
        cursor = queryResponse.getNextCursorMark();
      }
      log.debug("Done {}", batchNo++ * reindexBatchSize);
    }
    destinationCollection.commit();
  }

  @Override
  public void updateBatch(SolrClient solr, List<SolrInputDocument> solrInputDocuments)
      throws IOException, SolrServerException {
    update(solrInputDocuments, solr);
  }

  private SolrQuery getSolrQuery(List<String> categoryCodes, String sortOrder, int reindexBatchSize) {
    SolrQuery solrQuery = new SolrQuery();
    solrQuery.setFields(SolrFieldNames.PRODUCT_SKU, SolrFieldNames.PRODUCT_NAME, SolrFieldNames.IS_SUSPENDED,
        SolrFieldNames.ITEM_IMAGES, SolrFieldNames.IS_SYNCHRONIZED, SolrFieldNames.PRODUCT_CODE,
        SolrFieldNames.SALES_CATALOG, SolrFieldNames.MASTER_CATALOG, SolrFieldNames.BRAND, SolrFieldNames.CREATED_DATE,
        SolrFieldNames.UPDATED_DATE, SolrFieldNames.MERCHANT_CODE, SolrFieldNames.STORE_ID,
        SolrFieldNames.MARK_FOR_DELETE);
    solrQuery.setRows(reindexBatchSize);
    if (CollectionUtils.isNotEmpty(categoryCodes)) {
      solrQuery.setQuery(SolrFieldNames.MASTER_CATALOG + SolrConstants.COLON + getFQForListOfStrings(categoryCodes));
    } else {
      solrQuery.setQuery(QUERY);
    }
    if (SolrConstants.ASC.equalsIgnoreCase(sortOrder)) {
      solrQuery.addSort(SolrQuery.SortClause.asc(SolrFieldNames.CREATED_DATE));
      solrQuery.addSort(SolrQuery.SortClause.asc(SolrFieldNames.ID));
    } else {
      solrQuery.addSort(SolrQuery.SortClause.desc(SolrFieldNames.CREATED_DATE));
      solrQuery.addSort(SolrQuery.SortClause.desc(SolrFieldNames.ID));
    }
    solrQuery.addFilterQuery(SolrConstants.COLLAPSE_QUERY_FOR_PRODUCT_SKU);
    return solrQuery;
  }

  /**
   * Update solr documents in new collection
   *
   * @param solrInputDocumentList
   * @param solrClient
   */
  private void update(List<SolrInputDocument> solrInputDocumentList, SolrClient solrClient)
      throws IOException, SolrServerException {
    try {
      if (CollectionUtils.isNotEmpty(solrInputDocumentList)) {
        UpdateResponse updateResponse = solrClient.add(solrInputDocumentList);
        if (updateResponse.getStatus() != 0) {
          log.error(SOLR_UPDATE_ERROR_MESSAGE, updateResponse);
        }
      }
    } catch (Exception ex) {
      log.error(SOLR_UPDATE_ERROR_MESSAGE, ex);
      throw ex;
    }

  }

  /**
   * Convert SolrDocumentList into list of SolrInputDocument
   *
   * @param solrDocumentList
   * @param counter
   * @return
   */
  private List<SolrInputDocument> convertToInputDoc(SolrDocumentList solrDocumentList, Integer counter) {
    return solrDocumentList.stream().map(CommonUtil::toSolrInputDocumentFromSolrDocument).collect(Collectors.toList());
  }


  private List<SolrInputDocument> convertToL3SolrDocument(SolrDocumentList solrDocuments,
      Map<String, ProductAndItemsVO> productSkuAndProductAndItemsVOMap) {
    return solrDocuments.stream().map(solrDocument -> CommonUtil.toSolrInputDocumentL3(solrDocument,
        productSkuAndProductAndItemsVOMap
            .get((String) solrDocument.getFieldValue(SolrFieldNames.PRODUCT_SKU))))
        .collect(Collectors.toList());
  }

  /**
   * Get fields from existing collection schema
   *
   * @param solrClient
   * @return
   * @throws Exception
   */
  private String[] getFields(SolrClient solrClient) throws Exception {
    SolrQuery query = new SolrQuery();
    query.add(CommonParams.QT, SolrConstants.SEPARATOR + SolrConstants.SCHEMA);
    QueryResponse response = solrClient.query(query);
    Map<String,Object> map = (Map<String,Object>)response.getResponse().get(SolrConstants.SCHEMA);
    List<SimpleOrderedMap> copyFields = (List<SimpleOrderedMap>) map.get(SolrConstants.COPY_FIELDS);
    List<SimpleOrderedMap> dynamicFields = (List<SimpleOrderedMap>) map.get(SolrConstants.DYNAMIC_FIELDS);
    List<SimpleOrderedMap> fields = (List<SimpleOrderedMap>) map.get(SolrConstants.SOLR_FIELDS);
    List<String> solrFields = new ArrayList<>();
    if (CollectionUtils.isNotEmpty(fields)) {
      solrFields = fields.stream().map(field -> (String) field.get(SolrConstants.NAME)).collect(Collectors.toList());
    }
    if (CollectionUtils.isNotEmpty(dynamicFields)) {
      solrFields.addAll(
          dynamicFields.stream().map(field -> (String) field.get(SolrConstants.NAME)).collect(Collectors.toList()));
    }
    if (CollectionUtils.isNotEmpty(copyFields)) {
      solrFields.removeAll(
          copyFields.stream().map(field -> (String) field.get(SolrConstants.DEST)).collect(Collectors.toList()));
    }
    if (solrFields.contains(SolrConstants.VERSION)) {
      solrFields.remove(SolrConstants.VERSION);
    }
    if (solrFields.contains(SolrFieldNames.STORE_ID)) {
      solrFields.remove(SolrConstants.STORE_ID);
    }
    if (solrFields.contains(SolrFieldNames.MARK_FOR_DELETE)) {
      solrFields.remove(SolrConstants.MARK_FOR_DELETE);
    }
    return solrFields.toArray(new String[solrFields.size()]);
  }

  private String getFQForListOfStrings(List<String> strings) {
    return new StringBuilder().append(SolrConstants.OPEN_BRACKET).append(SolrConstants.DOUBLE_QUOTE)
        .append(String.join(SolrConstants.COMMA_WITH_QUOTES, strings)).append(SolrConstants.DOUBLE_QUOTE)
        .append(SolrConstants.CLOSING_BRACKET).toString();
  }
}
