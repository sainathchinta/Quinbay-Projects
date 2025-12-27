package com.gdn.mta.product.repository;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

import org.apache.commons.collections.CollectionUtils;
import org.apache.solr.client.solrj.SolrClient;
import org.apache.solr.client.solrj.SolrQuery;
import org.apache.solr.client.solrj.response.QueryResponse;
import org.apache.solr.client.solrj.response.UpdateResponse;
import org.apache.solr.common.SolrDocumentList;
import org.apache.solr.common.SolrInputDocument;
import org.apache.solr.common.params.CommonParams;
import org.apache.solr.common.params.CursorMarkParams;
import org.apache.solr.common.util.SimpleOrderedMap;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Repository;

import com.gdn.partners.pbp.commons.util.SolrConstants;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@Repository
public class SolrIndexingRepositoryBean implements SolrIndexingRepository {

  private static final String ID = "id";
  private static final String SOLR_UPDATE_ERROR_MESSAGE = "Error while updating the Documents to SOLR,";
  private static final String BATCHED_UPDATE_ERROR_MESSAGE =
      "Error occurred while updating documents batch number :{} ";
  private static final String QUERY = "id:*";

  @Value("${solr.indexing.batch.size:10000}")
  private int batchSize;

  @Override
  public void updateAll(SolrClient sourceCollection, SolrClient destinationCollection) throws Exception {
    SolrQuery solrQuery = new SolrQuery(QUERY);
    solrQuery.setSort(SolrQuery.SortClause.asc(ID));
    solrQuery.setFields(getFields(sourceCollection));
    solrQuery.setRows(batchSize);
    int batchNo = 1;
    String cursor = CursorMarkParams.CURSOR_MARK_START;
    boolean isLastBatch = false;
    while (!isLastBatch) {
      try {
        solrQuery.set(CursorMarkParams.CURSOR_MARK_PARAM, cursor);
        QueryResponse queryResponse = sourceCollection.query(solrQuery);
        if (Objects.nonNull(queryResponse) && CollectionUtils.isNotEmpty(queryResponse.getResults())) {
          update(convertToInputDoc(queryResponse.getResults(), null), destinationCollection);
        }
        isLastBatch = queryResponse.getNextCursorMark().equals(cursor);
        cursor = queryResponse.getNextCursorMark();
        log.info(cursor);
      } catch (Exception ex) {
        log.error(BATCHED_UPDATE_ERROR_MESSAGE, batchNo, ex);
      }
      log.info("Done {}", batchNo++ * batchSize);
    }
    destinationCollection.commit();
  }

  /**
   * Update solr documents in new collection
   *
   * @param solrInputDocumentList
   * @param solrClient
   */
  private void update(List<SolrInputDocument> solrInputDocumentList, SolrClient solrClient) {
    try {
      if (CollectionUtils.isNotEmpty(solrInputDocumentList)) {
        UpdateResponse updateResponse = solrClient.add(solrInputDocumentList);
        if (updateResponse.getStatus() != 0) {
          log.error(SOLR_UPDATE_ERROR_MESSAGE, updateResponse);
        }
      }
    } catch (Exception ex) {
      log.error(SOLR_UPDATE_ERROR_MESSAGE, ex);
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
    return solrDocumentList.stream().map(solrDocument -> {
      SolrInputDocument doc = new SolrInputDocument();
      solrDocument.forEach(doc::setField);
      return doc;
    }).collect(Collectors.toList());
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
    SimpleOrderedMap map = (SimpleOrderedMap) response.getResponse().get(SolrConstants.SCHEMA);
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
    return solrFields.toArray(new String[solrFields.size()]);
  }

}
