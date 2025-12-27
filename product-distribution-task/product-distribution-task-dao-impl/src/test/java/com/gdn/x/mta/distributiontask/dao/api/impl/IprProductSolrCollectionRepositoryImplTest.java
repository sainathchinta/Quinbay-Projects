package com.gdn.x.mta.distributiontask.dao.api.impl;

import com.gdn.x.mta.distributiontask.model.dto.IPRProductListRequest;
import com.gdn.x.mta.distributiontask.model.solr.IprProductSolrFieldNames;
import com.gdn.x.mta.distributiontask.model.enums.ProductStateIPR;
import com.gdn.x.mta.distributiontask.model.solr.IPRProductSolr;
import com.gdn.x.mta.distributiontask.model.solr.SolrConstants;
import org.apache.commons.lang3.StringUtils;
import org.apache.solr.client.solrj.SolrQuery;
import org.apache.solr.client.solrj.impl.CloudSolrClient;
import org.apache.solr.client.solrj.response.FacetField;
import org.apache.solr.client.solrj.response.IntervalFacet;
import org.apache.solr.client.solrj.response.QueryResponse;
import org.apache.solr.client.solrj.response.UpdateResponse;
import org.apache.solr.common.SolrDocument;
import org.apache.solr.common.SolrDocumentList;
import org.apache.solr.common.SolrInputDocument;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Map;

import static org.mockito.Mockito.times;
import static org.mockito.Mockito.when;


@ExtendWith(MockitoExtension.class)
class IprProductSolrCollectionRepositoryImplTest {
  private static final String STORE_ID = "storeId";
  private static final String PRODUCT_ID = "productId";
  private static final int PAGE = 0;
  private static final int SIZE = 100;
  private static final int FACET_COUNT = 10;
  private static final String PRODUCT_CODE = "MTA-173947046";
  private static final String PRODUCT_NAME = "productName";
  private static final String BUSINESS_PARTNER_CODE = "businessPartnerCode";
  private static final String CATEGORY_CODE = "categoryCode";
  private static final String BRAND_CODE = "brandCode";
  private static final String ASSIGNED_TO = "assignedTo";
  private static final String SOURCE = "source";

  @InjectMocks
  private IprProductSolrCollectionRepositoryImpl iprProductSolrCollectionRepository;

  @Mock
  @Qualifier(value = "iprProductCollectionClient")
  private CloudSolrClient cloudSolrClient;

  @Mock
  private QueryResponse queryResponse;

  @Mock
  private IntervalFacet intervalFacet;

  @Mock
  private List<IntervalFacet> intervalFacets;
  @Mock
  private IntervalFacet.Count count1;
  @Mock
  private IntervalFacet.Count count2;
  @Mock
  private IntervalFacet.Count count3;
  @Mock
  private IntervalFacet.Count count4;
  @Mock
  private FacetField facetField1;
  @Mock
  private FacetField.Count count6;
  @Mock
  private FacetField.Count count7;

  @Mock
  private FacetField.Count count8;

  @Captor
  private ArgumentCaptor<SolrInputDocument> solrInputDocumentArgumentCaptor;

  private IPRProductListRequest iprProductListRequest;
  private Pageable pageable;
  private SolrDocumentList solrDocumentList = new SolrDocumentList();

  private final IPRProductSolr iprProductSolr = new IPRProductSolr();

  @BeforeEach
  void setup() {
    iprProductListRequest = new IPRProductListRequest();
    SolrDocument solrDocument = new SolrDocument();
    solrDocumentList.add(solrDocument);
    pageable = PageRequest.of(PAGE, SIZE);
    iprProductSolr.setCategoryCode(CATEGORY_CODE);
    iprProductSolr.setState(ProductStateIPR.IN_REVIEW.name());
    iprProductSolr.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    iprProductSolr.setProductName(PRODUCT_NAME);
    iprProductSolr.setProductCode(PRODUCT_CODE);
    iprProductSolr.setBrandCode(BRAND_CODE);
    iprProductSolr.setSource(SOURCE);
  }

  @AfterEach
  void tearDown() {
    Mockito.verifyNoMoreInteractions(cloudSolrClient);
    Mockito.verifyNoMoreInteractions(queryResponse);
  }

  @Test
  void getIprProductsList() throws Exception {
    Mockito.when(cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    Mockito.when(queryResponse.getResults()).thenReturn(solrDocumentList);
    Page<IPRProductSolr> iprProductSolrList =
      iprProductSolrCollectionRepository.getIprProductsList(STORE_ID, iprProductListRequest,
        pageable);
    Mockito.verify(cloudSolrClient).query(Mockito.any(SolrQuery.class));
    Mockito.verify(queryResponse, Mockito.times(2)).getResults();
    Assertions.assertEquals(1, iprProductSolrList.getContent().size());
  }

  @Test
  void testAddDocumentToIPRSolr() throws Exception {
    Mockito.when(this.cloudSolrClient.add(Mockito.anyList())).thenReturn(new UpdateResponse());
    iprProductSolrCollectionRepository.addDocumentToIPRSolr(iprProductSolr);
    Mockito.verify(this.cloudSolrClient).add(solrInputDocumentArgumentCaptor.capture());
    Assertions.assertNotNull(solrInputDocumentArgumentCaptor.getValue());
    Assertions.assertEquals(BUSINESS_PARTNER_CODE, solrInputDocumentArgumentCaptor.getValue()
        .getField(IprProductSolrFieldNames.BUSINESS_PARTNER_CODE).getValue());
    Assertions.assertEquals(PRODUCT_CODE,
        solrInputDocumentArgumentCaptor.getValue().getField(IprProductSolrFieldNames.PRODUCT_CODE)
            .getValue());
    Assertions.assertEquals(PRODUCT_NAME,
        solrInputDocumentArgumentCaptor.getValue().getField(IprProductSolrFieldNames.PRODUCT_NAME)
            .getValue());
    Assertions.assertEquals(BRAND_CODE,
        solrInputDocumentArgumentCaptor.getValue().getField(IprProductSolrFieldNames.BRAND_CODE)
            .getValue());
    Assertions.assertEquals(ProductStateIPR.IN_REVIEW.getValue(),
        solrInputDocumentArgumentCaptor.getValue().getField(IprProductSolrFieldNames.STATE)
            .getValue());
    Assertions.assertEquals(SOURCE,
        solrInputDocumentArgumentCaptor.getValue().getField(IprProductSolrFieldNames.SOURCE)
            .getName());
  }

  @Test
  void testAddDocumentToIPRSolrAssignedTo() throws Exception {
    iprProductSolr.setAssignedTo(ASSIGNED_TO);
    Mockito.when(this.cloudSolrClient.add(Mockito.anyList())).thenReturn(new UpdateResponse());
    iprProductSolrCollectionRepository.addDocumentToIPRSolr(iprProductSolr);
    Mockito.verify(this.cloudSolrClient).add(solrInputDocumentArgumentCaptor.capture());
    Assertions.assertNotNull(solrInputDocumentArgumentCaptor.getValue());
    Assertions.assertEquals(BUSINESS_PARTNER_CODE, solrInputDocumentArgumentCaptor.getValue()
        .getField(IprProductSolrFieldNames.BUSINESS_PARTNER_CODE).getValue());
    Assertions.assertEquals(PRODUCT_CODE,
        solrInputDocumentArgumentCaptor.getValue().getField(IprProductSolrFieldNames.PRODUCT_CODE)
            .getValue());
    Assertions.assertEquals(PRODUCT_NAME,
        solrInputDocumentArgumentCaptor.getValue().getField(IprProductSolrFieldNames.PRODUCT_NAME)
            .getValue());
    Assertions.assertEquals(ASSIGNED_TO,
        solrInputDocumentArgumentCaptor.getValue().getField(IprProductSolrFieldNames.ASSIGNED_TO)
            .getValue());
    Assertions.assertEquals(BRAND_CODE,
        solrInputDocumentArgumentCaptor.getValue().getField(IprProductSolrFieldNames.BRAND_CODE)
            .getValue());
    Assertions.assertEquals(ProductStateIPR.IN_REVIEW.getValue(),
        solrInputDocumentArgumentCaptor.getValue().getField(IprProductSolrFieldNames.STATE)
            .getValue());
  }

  @Test
  void testAddDocumentToIPRSolr_exception() throws Exception {
    Mockito.when(this.cloudSolrClient.add(Mockito.anyList())).thenThrow(RuntimeException.class);
    try {
      iprProductSolrCollectionRepository.addDocumentToIPRSolr(iprProductSolr);
    } finally {
      Mockito.verify(this.cloudSolrClient).add(solrInputDocumentArgumentCaptor.capture());
    }
  }

  @Test
  void testAddDocumentToIPRSolr_WithNull() throws Exception {
    iprProductSolrCollectionRepository.addDocumentToIPRSolr(null);
    Mockito.verify(cloudSolrClient, Mockito.never()).add(Mockito.anyList());
  }

  @Test
  void deleteIprProductTest() throws Exception {
    Mockito.when(cloudSolrClient.deleteByQuery(Mockito.anyString())).thenReturn(new UpdateResponse());
    iprProductSolrCollectionRepository.deleteIprSolrDocument(PRODUCT_ID);
    Mockito.verify(cloudSolrClient).deleteByQuery(Mockito.anyString());
  }

  @Test
  void deleteIprProductEmptyIdTest() throws Exception {
    iprProductSolrCollectionRepository.deleteIprSolrDocument(StringUtils.EMPTY);
    Mockito.verify(cloudSolrClient, Mockito.never()).deleteByQuery(Mockito.anyString());
  }

  @Test
  void deleteIprProductExceptionTest() throws Exception {
    Mockito.when(cloudSolrClient.deleteByQuery(Mockito.anyString())).thenThrow(RuntimeException.class);
    try {
      iprProductSolrCollectionRepository.deleteIprSolrDocument(PRODUCT_ID);
    } finally {
      Mockito.verify(cloudSolrClient).deleteByQuery(Mockito.anyString());
    }
  }

  @Test
  void getPrimaryFilterCountsTest() throws Exception {
    Mockito.when(cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    when(this.queryResponse.getIntervalFacets()).thenReturn(intervalFacets);
    when(this.intervalFacets.get(0)).thenReturn(intervalFacet);
    when(this.queryResponse.getFacetFields()).thenReturn(Collections.singletonList(facetField1));
    when(this.intervalFacet.getIntervals()).thenReturn(
      Arrays.asList(count1, count2, count3, count4));
    when(this.count1.getCount()).thenReturn(FACET_COUNT);
    when(this.count2.getCount()).thenReturn(FACET_COUNT);
    when(this.count3.getCount()).thenReturn(FACET_COUNT);
    when(this.count4.getCount()).thenReturn(FACET_COUNT);
    when(this.count1.getKey()).thenReturn(SolrConstants.TODAY_FACET_INTERVAL);
    when(this.count2.getKey()).thenReturn(SolrConstants.YESTERDAY_FACET_INTERVAL);
    when(this.count3.getKey()).thenReturn(SolrConstants.TWO_TO_THREE_DAYS_AGO_FACET_INTERVAL);
    when(this.count4.getKey()).thenReturn(SolrConstants.THREE_DAYS_AGO_FACET_INTERVAL);
    when(this.facetField1.getName()).thenReturn(IprProductSolrFieldNames.STATE);
    when(this.facetField1.getValues()).thenReturn(Arrays.asList(count6, count7, count8));
    when(this.count6.getName()).thenReturn(String.valueOf(ProductStateIPR.IN_REVIEW.getValue()));
    when(this.count7.getName()).thenReturn(
      String.valueOf(ProductStateIPR.EVIDENCE_SUBMITTED.getValue()));
    when(this.count8.getName()).thenReturn(
      String.valueOf(ProductStateIPR.EVIDENCE_REQUESTED.getValue()));
    Map<String, Object> filterCounts =
      this.iprProductSolrCollectionRepository.getPrimaryFilterCounts(STORE_ID);
    Mockito.verify(this.queryResponse).getIntervalFacets();
    Mockito.verify(this.queryResponse).getFacetFields();
    Mockito.verify(this.queryResponse).getFacetQuery();
    Mockito.verify(this.intervalFacets).get(0);
    Mockito.verify(this.intervalFacet).getIntervals();
    Mockito.verify(this.count1).getCount();
    Mockito.verify(this.count2).getCount();
    Mockito.verify(this.count3).getCount();
    Mockito.verify(this.count4).getCount();
    Mockito.verify(this.count1).getKey();
    Mockito.verify(this.count2).getKey();
    Mockito.verify(this.count3).getKey();
    Mockito.verify(this.count4).getKey();
    Mockito.verify(this.facetField1).getValues();
    Mockito.verify(this.count6).getName();
    Mockito.verify(this.count7, times(2)).getName();
    Mockito.verify(this.count8, times(2)).getName();
    Assertions.assertEquals(9, filterCounts.size());
  }

  @Test
  void getPrimaryFilterCountsNullResponseTest() throws Exception {
    Mockito.when(cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenReturn(null);
    Map<String, Object> filterCounts =
      this.iprProductSolrCollectionRepository.getPrimaryFilterCounts(STORE_ID);
    Assertions.assertEquals(0, filterCounts.size());
  }

  @Test
  void getPrimaryFilterCountsInvalidFacetFieldResponseTest() throws Exception {
    Mockito.when(cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    when(this.queryResponse.getIntervalFacets()).thenReturn(intervalFacets);
    when(this.intervalFacets.get(0)).thenReturn(intervalFacet);
    when(this.queryResponse.getFacetFields()).thenReturn(Collections.singletonList(facetField1));
    when(this.intervalFacet.getIntervals()).thenReturn(
      Arrays.asList(count1, count2, count3, count4));
    when(this.count1.getCount()).thenReturn(FACET_COUNT);
    when(this.count2.getCount()).thenReturn(FACET_COUNT);
    when(this.count3.getCount()).thenReturn(FACET_COUNT);
    when(this.count4.getCount()).thenReturn(FACET_COUNT);
    when(this.count1.getKey()).thenReturn(SolrConstants.TODAY_FACET_INTERVAL);
    when(this.count2.getKey()).thenReturn(SolrConstants.YESTERDAY_FACET_INTERVAL);
    when(this.count3.getKey()).thenReturn(SolrConstants.TWO_TO_THREE_DAYS_AGO_FACET_INTERVAL);
    when(this.count4.getKey()).thenReturn(SolrConstants.THREE_DAYS_AGO_FACET_INTERVAL);
    when(this.facetField1.getName()).thenReturn(IprProductSolrFieldNames.CATEGORY_NAME);
    Map<String, Object> filterCounts =
      this.iprProductSolrCollectionRepository.getPrimaryFilterCounts(STORE_ID);
    Mockito.verify(this.queryResponse).getIntervalFacets();
    Mockito.verify(this.queryResponse).getFacetFields();
    Mockito.verify(this.queryResponse).getFacetQuery();
    Mockito.verify(this.intervalFacets).get(0);
    Mockito.verify(this.intervalFacet).getIntervals();
    Mockito.verify(this.count1).getCount();
    Mockito.verify(this.count2).getCount();
    Mockito.verify(this.count3).getCount();
    Mockito.verify(this.count4).getCount();
    Mockito.verify(this.count1).getKey();
    Mockito.verify(this.count2).getKey();
    Mockito.verify(this.count3).getKey();
    Mockito.verify(this.count4).getKey();
    Mockito.verify(this.facetField1).getName();
    Assertions.assertEquals(7, filterCounts.size());
  }
}
