package com.gdn.mta.product.repository;

import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.List;

import org.apache.solr.client.solrj.SolrClient;
import org.apache.solr.client.solrj.SolrQuery;
import org.apache.solr.client.solrj.impl.CloudSolrClient;
import org.apache.solr.client.solrj.response.QueryResponse;
import org.apache.solr.client.solrj.response.UpdateResponse;
import org.apache.solr.common.SolrDocument;
import org.apache.solr.common.SolrDocumentList;
import org.apache.solr.common.SolrInputDocument;
import org.apache.solr.common.util.NamedList;
import org.apache.solr.common.util.SimpleOrderedMap;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Qualifier;

import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.partners.pbp.commons.util.SolrConstants;
import com.gdn.search.searchservice.model.SolrFieldNames;

public class SolrIndexingRepositoryBeanTest {

  @InjectMocks
  private SolrIndexingRepositoryBean solrIndexingRepositoryBean;

  @Mock
  private QueryResponse queryResponse;

  @Mock
  private SolrClient sourceClient;

  @Mock
  private SolrClient destinationClient;

  @Mock
  private NamedList namedList;

  @Mock
  private SimpleOrderedMap map;

  @Mock
  private UpdateResponse updateResponse;

  @Captor
  private ArgumentCaptor<SolrQuery> solrQueryCaptor;

  @Captor
  private ArgumentCaptor<List<SolrInputDocument>> listArgumentCaptor;

  private List<SolrInputDocument> solrInputDocuments;
  private SolrInputDocument solrInputDocument;
  private List<SimpleOrderedMap> maps;
  private SimpleOrderedMap simpleOrderedMap;
  private SolrDocumentList solrDocumentList;
  private SolrDocumentList solrDocumentList1;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    SolrDocument solrDocument = new SolrDocument();
    solrDocument.addField(SolrFieldNames.NAME, Constants.DEFAULT_USERNAME);
    solrDocumentList = new SolrDocumentList();
    solrDocumentList.add(solrDocument);
    simpleOrderedMap = new SimpleOrderedMap();
    simpleOrderedMap.add(SolrConstants.NAME, Constants.PRODUCT_NAME);
    simpleOrderedMap.add(SolrConstants.VERSION, Constants.NOT_APPLICABLE);
    solrInputDocuments = new ArrayList<>();
    solrInputDocument = new SolrInputDocument();
    solrInputDocument.addField(SolrFieldNames.NAME, Constants.DEFAULT_USERNAME);
    solrInputDocuments.add(solrInputDocument);
    maps = new ArrayList<>();
    maps.add(simpleOrderedMap);
  }

  @AfterEach
  public void tearDown() throws Exception {
    verifyNoMoreInteractions(queryResponse, map, destinationClient, sourceClient);
  }

  @Test
  public void updateAllTest() throws Exception {
    when(this.queryResponse.getResults()).thenReturn(solrDocumentList);
    when(this.queryResponse.getNextCursorMark()).thenReturn("50").thenReturn("50");
    when(this.sourceClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    when(this.queryResponse.getResponse()).thenReturn(namedList);
    when(this.namedList.get(SolrConstants.SCHEMA)).thenReturn(map);
    when(this.map.get(SolrConstants.COPY_FIELDS)).thenReturn(new ArrayList<SimpleOrderedMap>());
    when(this.map.get(SolrConstants.DYNAMIC_FIELDS)).thenReturn(new ArrayList<SimpleOrderedMap>());
    when(this.map.get(SolrConstants.SOLR_FIELDS)).thenReturn(maps);
    when(this.destinationClient.add(Mockito.anyList())).thenReturn(updateResponse);
    when(this.updateResponse.getStatus()).thenReturn(0);
    when(this.destinationClient.commit()).thenReturn(updateResponse);
    this.solrIndexingRepositoryBean.updateAll(sourceClient, destinationClient);
    verify(this.queryResponse, times(4)).getResults();
    verify(this.queryResponse, times(4)).getNextCursorMark();
    verify(this.queryResponse).getResponse();
    verify(this.sourceClient, times(3)).query(solrQueryCaptor.capture());
    verify(this.destinationClient).commit();
    verify(this.destinationClient, times(2)).add(listArgumentCaptor.capture());
    verify(this.updateResponse, times(2)).getStatus();
    verify(this.map).get(SolrConstants.SOLR_FIELDS);
    verify(this.map).get(SolrConstants.DYNAMIC_FIELDS);
    verify(this.map).get(SolrConstants.COPY_FIELDS);
    verify(this.namedList).get(SolrConstants.SCHEMA);
  }

  @Test
  public void updateAllCopyFieldsAndDynamicFieldsTest() throws Exception {
    when(this.queryResponse.getResults()).thenReturn(solrDocumentList);
    when(this.queryResponse.getNextCursorMark()).thenReturn("50").thenReturn("50");
    when(this.sourceClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    when(this.queryResponse.getResponse()).thenReturn(namedList);
    when(this.namedList.get(SolrConstants.SCHEMA)).thenReturn(map);
    when(this.map.get(SolrConstants.COPY_FIELDS)).thenReturn(maps);
    when(this.map.get(SolrConstants.DYNAMIC_FIELDS)).thenReturn(maps);
    when(this.map.get(SolrConstants.SOLR_FIELDS)).thenReturn(new ArrayList<SimpleOrderedMap>());
    when(this.destinationClient.add(Mockito.anyList())).thenReturn(updateResponse);
    when(this.updateResponse.getStatus()).thenReturn(0);
    when(this.destinationClient.commit()).thenReturn(updateResponse);
    this.solrIndexingRepositoryBean.updateAll(sourceClient, destinationClient);
    verify(this.queryResponse, times(4)).getResults();
    verify(this.queryResponse, times(4)).getNextCursorMark();
    verify(this.queryResponse).getResponse();
    verify(this.sourceClient, times(3)).query(solrQueryCaptor.capture());
    verify(this.destinationClient).commit();
    verify(this.destinationClient, times(2)).add(listArgumentCaptor.capture());
    verify(this.updateResponse, times(2)).getStatus();
    verify(this.map).get(SolrConstants.SOLR_FIELDS);
    verify(this.map).get(SolrConstants.DYNAMIC_FIELDS);
    verify(this.map).get(SolrConstants.COPY_FIELDS);
    verify(this.namedList).get(SolrConstants.SCHEMA);
  }

  @Test
  public void updateAllStatusNonZeroTest() throws Exception {
    when(this.queryResponse.getResults()).thenReturn(solrDocumentList);
    when(this.queryResponse.getNextCursorMark()).thenReturn("50").thenReturn("50");
    when(this.sourceClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    when(this.queryResponse.getResponse()).thenReturn(namedList);
    when(this.namedList.get(SolrConstants.SCHEMA)).thenReturn(map);
    when(this.map.get(SolrConstants.COPY_FIELDS)).thenReturn(maps);
    when(this.map.get(SolrConstants.DYNAMIC_FIELDS)).thenReturn(maps);
    when(this.map.get(SolrConstants.SOLR_FIELDS)).thenReturn(new ArrayList<SimpleOrderedMap>());
    when(this.destinationClient.add(Mockito.anyList())).thenReturn(updateResponse);
    when(this.updateResponse.getStatus()).thenReturn(1);
    when(this.destinationClient.commit()).thenReturn(updateResponse);
    this.solrIndexingRepositoryBean.updateAll(sourceClient, destinationClient);
    verify(this.queryResponse, times(4)).getResults();
    verify(this.queryResponse, times(4)).getNextCursorMark();
    verify(this.queryResponse).getResponse();
    verify(this.sourceClient, times(3)).query(solrQueryCaptor.capture());
    verify(this.destinationClient).commit();
    verify(this.destinationClient, times(2)).add(listArgumentCaptor.capture());
    verify(this.updateResponse, times(2)).getStatus();
    verify(this.map).get(SolrConstants.SOLR_FIELDS);
    verify(this.map).get(SolrConstants.DYNAMIC_FIELDS);
    verify(this.map).get(SolrConstants.COPY_FIELDS);
    verify(this.namedList).get(SolrConstants.SCHEMA);
  }

  @Test
  public void updateAllResponseExceptionTest() throws Exception {
    when(this.queryResponse.getResults()).thenReturn(solrDocumentList);
    when(this.queryResponse.getNextCursorMark()).thenReturn("50").thenReturn("50");
    when(this.sourceClient.query(Mockito.any())).thenReturn(queryResponse);
    when(this.queryResponse.getResponse()).thenReturn(namedList);
    when(this.namedList.get(SolrConstants.SCHEMA)).thenReturn(map);
    when(this.map.get(SolrConstants.COPY_FIELDS)).thenReturn(maps);
    when(this.map.get(SolrConstants.DYNAMIC_FIELDS)).thenReturn(maps);
    when(this.map.get(SolrConstants.SOLR_FIELDS)).thenReturn(new ArrayList<SimpleOrderedMap>());
    when(this.destinationClient.add(Mockito.anyList())).thenThrow(RuntimeException.class);
    when(this.destinationClient.commit()).thenReturn(updateResponse);
    this.solrIndexingRepositoryBean.updateAll(sourceClient, destinationClient);
    verify(this.queryResponse, times(4)).getResults();
    verify(this.queryResponse, times(4)).getNextCursorMark();
    verify(this.queryResponse).getResponse();
    verify(this.sourceClient, times(3)).query(solrQueryCaptor.capture());
    verify(this.destinationClient).commit();
    verify(this.destinationClient, times(2)).add(listArgumentCaptor.capture());
    verify(this.map).get(SolrConstants.SOLR_FIELDS);
    verify(this.map).get(SolrConstants.DYNAMIC_FIELDS);
    verify(this.map).get(SolrConstants.COPY_FIELDS);
    verify(this.namedList).get(SolrConstants.SCHEMA);
  }

  @Test
  public void updateAllExceptionTest() throws Exception {
    when(this.queryResponse.getResults()).thenReturn(solrDocumentList);
    when(this.queryResponse.getNextCursorMark()).thenReturn("50").thenReturn("50").thenReturn("50")
        .thenThrow(RuntimeException.class);
    when(this.sourceClient.query(Mockito.any())).thenReturn(queryResponse);
    when(this.queryResponse.getResponse()).thenReturn(namedList);
    when(this.namedList.get(SolrConstants.SCHEMA)).thenReturn(map);
    when(this.map.get(SolrConstants.COPY_FIELDS)).thenReturn(maps);
    when(this.map.get(SolrConstants.DYNAMIC_FIELDS)).thenReturn(maps);
    when(this.map.get(SolrConstants.SOLR_FIELDS)).thenReturn(new ArrayList<SimpleOrderedMap>());
    when(this.destinationClient.commit()).thenReturn(updateResponse);
    this.solrIndexingRepositoryBean.updateAll(sourceClient, destinationClient);
    verify(this.queryResponse, times(4)).getResults();
    verify(this.queryResponse, times(4)).getNextCursorMark();
    verify(this.queryResponse).getResponse();
    verify(this.sourceClient, times(3)).query(solrQueryCaptor.capture());
    verify(this.destinationClient, times(2)).add(listArgumentCaptor.capture());
    verify(this.map).get(SolrConstants.SOLR_FIELDS);
    verify(this.map).get(SolrConstants.DYNAMIC_FIELDS);
    verify(this.map).get(SolrConstants.COPY_FIELDS);
    verify(this.namedList).get(SolrConstants.SCHEMA);
    verify(this.destinationClient).commit();
  }
}