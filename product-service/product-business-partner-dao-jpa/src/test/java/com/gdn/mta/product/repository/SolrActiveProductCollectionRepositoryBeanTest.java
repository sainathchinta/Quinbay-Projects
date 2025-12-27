package com.gdn.mta.product.repository;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Date;
import java.util.List;
import java.util.Set;
import java.util.UUID;

import com.gdn.partners.pbp.outbound.product.ProductOutbound;
import com.gdn.x.businesspartner.dto.CompanyDTO;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;
import org.apache.commons.lang3.StringUtils;
import org.apache.solr.client.solrj.SolrQuery;
import org.apache.solr.client.solrj.SolrServerException;
import org.apache.solr.client.solrj.impl.CloudSolrClient;
import org.apache.solr.client.solrj.response.Group;
import org.apache.solr.client.solrj.response.GroupCommand;
import org.apache.solr.client.solrj.response.GroupResponse;
import org.apache.solr.client.solrj.response.QueryResponse;
import org.apache.solr.client.solrj.response.UpdateResponse;
import org.apache.solr.common.SolrDocument;
import org.apache.solr.common.SolrDocumentList;
import org.apache.solr.common.SolrException;
import org.apache.solr.common.SolrInputDocument;
import org.apache.solr.common.params.CommonParams;
import org.apache.solr.common.util.NamedList;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.test.util.ReflectionTestUtils;

import com.gda.mta.product.dto.ProductFilterRequest;
import com.gdn.common.web.param.PageableHelper;
import com.gdn.mta.product.valueobject.SolrCategoryCodeDTO;
import com.gdn.mta.product.valueobject.SolrProductCodeDTO;
import com.gdn.mta.product.valueobject.SolrProductCollectionDTO;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.partners.pbp.commons.util.SolrFieldNames;

public class SolrActiveProductCollectionRepositoryBeanTest {

  private static final String STORE_ID = "10001";
  private static final String PRODUCT_ID = UUID.randomUUID().toString();
  private static final String PRODUCT_CODE = "MTA-012345";
  private static final String PRODUCT_NAME = "iphone7";
  private static final String BRAND = "apple";
  private static final String CATEGORY_CODE = "CD-10230";
  private static final String CATEGORY_CODE_FIELD = "category_code";
  private static final String BP_CODE = "BP-01234";
  private static final String USER_NAME = "viraj";
  private static final String PARENTHESES_OPEN = "(";
  private static final String DOUBLE_QUOTE = "\"";
  private static final String PARENTHESES_CLOSE = ")";
  private static final UpdateResponse UPDATE_RESPONSE = new UpdateResponse();
  private static final String KEYWORD = "apple";
  private static final int NUM_FOUND = 1;
  private static final String EDISMAX_PARAMETER = "edismax";
  private static final String REVIEW_PENDING = "review_pending";
  private static final String SORT_BY = "sortBy";
  private static final String DESC = "desc";
  private ProductFilterRequest productFilterRequest;

  @Mock
  @Qualifier(value = "prdCollectionClient")
  private CloudSolrClient prdCollectionClient;

  @Mock
  private ProductOutbound productOutbound;

  @Mock
  private BusinessPartnerRepository businessPartnerRepository;

  @InjectMocks
  private SolrActiveProductCollectionRepositoryBean solrActiveProductCollectionRepositoryBean;

  @Captor
  private ArgumentCaptor<SolrQuery> solrQueryArgumentCaptor;

  private List<String> categoryCodes = new ArrayList<>();

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    ReflectionTestUtils.setField(solrActiveProductCollectionRepositoryBean, "batchSize", "100");
    productFilterRequest =
        ProductFilterRequest.builder().productCode(PRODUCT_CODE).productName(PRODUCT_NAME).categoryCode(CATEGORY_CODE)
            .brandName(BRAND).build();
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(prdCollectionClient);
  }

  @Test
  public void getProductCollectionListFromSolrCollectionTest() throws Exception {
    Date date = Calendar.getInstance().getTime();
    String docId = UUID.randomUUID().toString();
    SolrDocument solrDocument = getSolrDocument(date, docId);
    SolrDocumentList solrDocumentList = new SolrDocumentList();
    solrDocumentList.add(solrDocument);
    QueryResponse queryResponse = new QueryResponse();
    ReflectionTestUtils
        .setField(queryResponse, QueryResponse.class, "_results", solrDocumentList, SolrDocumentList.class);
    queryResponse.getResults().setNumFound(100);
    Mockito.when(prdCollectionClient.query(Mockito.any())).thenReturn(queryResponse);
    Page<SolrProductCollectionDTO> solrProductCollectionDTOPage = solrActiveProductCollectionRepositoryBean
        .getProductCollectionListFromSolrCollection(STORE_ID, "012345", CATEGORY_CODE, null, SORT_BY,
            PageRequest.of(0, 1));
    Mockito.verify(prdCollectionClient).query(Mockito.any());
    checkAssertionForProductCollectionList(date, docId, solrProductCollectionDTOPage);
  }

  @Test
  public void getProductCollectionListFromSolrCollectionWithReviewPendingTrueTest() throws Exception {
    Date date = Calendar.getInstance().getTime();
    String docId = UUID.randomUUID().toString();
    SolrDocument solrDocument = getSolrDocument(date, docId);
    solrDocument.addField(REVIEW_PENDING, true);
    SolrDocumentList solrDocumentList = new SolrDocumentList();
    solrDocumentList.add(solrDocument);
    QueryResponse queryResponse = new QueryResponse();
    ReflectionTestUtils
        .setField(queryResponse, QueryResponse.class, "_results", solrDocumentList, SolrDocumentList.class);
    queryResponse.getResults().setNumFound(100);
    Mockito.when(prdCollectionClient.query(Mockito.any())).thenReturn(queryResponse);
    Page<SolrProductCollectionDTO> solrProductCollectionDTOPage = solrActiveProductCollectionRepositoryBean
        .getProductCollectionListFromSolrCollection(STORE_ID, "012345", CATEGORY_CODE, true, SORT_BY,
            PageRequest.of(0, 1));
    Mockito.verify(prdCollectionClient).query(Mockito.any());
    checkAssertionForProductCollectionList(date, docId, solrProductCollectionDTOPage);
    Assertions.assertTrue(solrProductCollectionDTOPage.getContent().get(0).isReviewPending());
  }

  @Test
  public void getProductCountFromSolrCollectionWithStoreId() throws Exception {
    Date date = Calendar.getInstance().getTime();
    String docId = UUID.randomUUID().toString();
    SolrDocument solrDocument = getSolrDocument(date, docId);
    SolrDocumentList solrDocumentList = new SolrDocumentList();
    solrDocumentList.add(solrDocument);
    QueryResponse queryResponse = new QueryResponse();
    ReflectionTestUtils
        .setField(queryResponse, QueryResponse.class, "_results", solrDocumentList, SolrDocumentList.class);
    queryResponse.getResults().setNumFound(535);
    Mockito.when(prdCollectionClient.query(Mockito.any())).thenReturn(queryResponse);
    Integer productCount = solrActiveProductCollectionRepositoryBean.getProductCountByStoreId(STORE_ID);
    Mockito.verify(prdCollectionClient).query(Mockito.any());
    Assertions.assertEquals(535, productCount);
  }

  @Test
  public void getProductCollectionListFromSolrCollectionWithReviewPendingFalseTest() throws Exception {
    Date date = Calendar.getInstance().getTime();
    String docId = UUID.randomUUID().toString();
    SolrDocument solrDocument = getSolrDocument(date, docId);
    solrDocument.addField(REVIEW_PENDING, false);
    SolrDocumentList solrDocumentList = new SolrDocumentList();
    solrDocumentList.add(solrDocument);
    QueryResponse queryResponse = new QueryResponse();
    ReflectionTestUtils
        .setField(queryResponse, QueryResponse.class, "_results", solrDocumentList, SolrDocumentList.class);
    queryResponse.getResults().setNumFound(100);
    Mockito.when(prdCollectionClient.query(Mockito.any())).thenReturn(queryResponse);
    Page<SolrProductCollectionDTO> solrProductCollectionDTOPage = solrActiveProductCollectionRepositoryBean
        .getProductCollectionListFromSolrCollection(STORE_ID, "012345", CATEGORY_CODE, true, SORT_BY,
            PageRequest.of(0, 1));
    Mockito.verify(prdCollectionClient).query(Mockito.any());
    checkAssertionForProductCollectionList(date, docId, solrProductCollectionDTOPage);
    Assertions.assertFalse(solrProductCollectionDTOPage.getContent().get(0).isReviewPending());
  }

  @Test
  public void getProductCodesListTest() throws Exception {
    Date date = Calendar.getInstance().getTime();
    String docId = UUID.randomUUID().toString();
    SolrDocument solrDocument = getSolrDocument(date, docId);
    SolrDocumentList solrDocumentList = new SolrDocumentList();
    solrDocumentList.add(solrDocument);
    QueryResponse queryResponse = new QueryResponse();
    ReflectionTestUtils
        .setField(queryResponse, QueryResponse.class, "_results", solrDocumentList, SolrDocumentList.class);
    Mockito.when(prdCollectionClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    List<String> productCodes = solrActiveProductCollectionRepositoryBean
        .getProductCodesListFromSolr(STORE_ID, "012345", CATEGORY_CODE, false, DESC,
            PageRequest.of(0, 1));
    Mockito.verify(prdCollectionClient).query(Mockito.any(SolrQuery.class));
    Assertions.assertTrue(productCodes.size() == 1);
    Assertions.assertTrue(productCodes.get(0) == PRODUCT_CODE);
  }

  @Test
  public void getProductCodesListBlankKeywordTest() throws Exception {
    Date date = Calendar.getInstance().getTime();
    String docId = UUID.randomUUID().toString();
    SolrDocument solrDocument = getSolrDocument(date, docId);
    SolrDocumentList solrDocumentList = new SolrDocumentList();
    solrDocumentList.add(solrDocument);
    QueryResponse queryResponse = new QueryResponse();
    ReflectionTestUtils
        .setField(queryResponse, QueryResponse.class, "_results", solrDocumentList, SolrDocumentList.class);
    Mockito.when(prdCollectionClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    List<String> productCodes = solrActiveProductCollectionRepositoryBean
        .getProductCodesListFromSolr(STORE_ID, "", CATEGORY_CODE, false, DESC,
            PageRequest.of(0, 1));
    Mockito.verify(prdCollectionClient).query(Mockito.any(SolrQuery.class));
    Assertions.assertTrue(productCodes.size() == 1);
    Assertions.assertTrue(productCodes.get(0) == PRODUCT_CODE);
  }

  @Test
  public void getProductCodesListTestWithException() throws Exception {
    Date date = Calendar.getInstance().getTime();
    String docId = UUID.randomUUID().toString();
    SolrDocument solrDocument = getSolrDocument(date, docId);
    SolrDocumentList solrDocumentList = new SolrDocumentList();
    solrDocumentList.add(solrDocument);
    QueryResponse queryResponse = new QueryResponse();
    ReflectionTestUtils
        .setField(queryResponse, QueryResponse.class, "_results", solrDocumentList, SolrDocumentList.class);
    Mockito.when(prdCollectionClient.query(Mockito.any(SolrQuery.class))).thenThrow(IOException.class);
    List<String> productCodes = solrActiveProductCollectionRepositoryBean
        .getProductCodesListFromSolr(STORE_ID, "012345", CATEGORY_CODE, true, DESC,
            PageRequest.of(0, 1));
    Mockito.verify(prdCollectionClient).query(Mockito.any(SolrQuery.class));
    Assertions.assertTrue(productCodes.size() == 0);
  }

  @Test
  public void getProductCollectionListFromSolrCollectionWithoutKeywordAndCategoryCodeTest() throws Exception {
    Date date = Calendar.getInstance().getTime();
    String docId = UUID.randomUUID().toString();
    SolrDocument solrDocument = getSolrDocument(date, docId);
    SolrDocumentList solrDocumentList = new SolrDocumentList();
    solrDocumentList.add(solrDocument);
    QueryResponse queryResponse = new QueryResponse();
    ReflectionTestUtils
        .setField(queryResponse, QueryResponse.class, "_results", solrDocumentList, SolrDocumentList.class);
    queryResponse.getResults().setNumFound(100);
    Mockito.when(prdCollectionClient.query(Mockito.any())).thenReturn(queryResponse);
    Page<SolrProductCollectionDTO> solrProductCollectionDTOPage = solrActiveProductCollectionRepositoryBean
        .getProductCollectionListFromSolrCollection(STORE_ID, null, null, null, SORT_BY,
            PageRequest.of(0, 1));
    Mockito.verify(prdCollectionClient).query(Mockito.any());
    checkAssertionForProductCollectionList(date, docId, solrProductCollectionDTOPage);
  }

  private void checkAssertionForProductCollectionList(Date date, String docId,
      Page<SolrProductCollectionDTO> solrProductCollectionDTOPage) {
    Assertions.assertEquals(solrProductCollectionDTOPage.getContent().size(), 1);
    Assertions.assertEquals(100, solrProductCollectionDTOPage.getTotalElements());
    Assertions.assertEquals(solrProductCollectionDTOPage.getContent().get(0).getId(), docId);
    Assertions.assertEquals(solrProductCollectionDTOPage.getContent().get(0).getProductCode(), PRODUCT_CODE);
    Assertions.assertEquals(solrProductCollectionDTOPage.getContent().get(0).getProductId(), PRODUCT_ID);
    Assertions.assertEquals(solrProductCollectionDTOPage.getContent().get(0).getUpdatedStepDate(), date);
    Assertions.assertEquals(1, solrProductCollectionDTOPage.getContent().size());
    Assertions.assertEquals(solrProductCollectionDTOPage.getContent().get(0).getCreatedBy(), USER_NAME);
  }

  @Test
  public void getProductCollectionListFromSolrCollection_ExceptionTest() throws Exception {
    Mockito.doThrow(SolrServerException.class).when(prdCollectionClient).query(Mockito.any());
    try {
      Assertions.assertThrows(SolrServerException.class, () -> {
        solrActiveProductCollectionRepositoryBean
            .getProductCollectionListFromSolrCollection(STORE_ID, "012345", CATEGORY_CODE, null, SORT_BY,
                PageRequest.of(0, 1));
      });
    } catch (Exception e) {
      throw e;
    }
    Mockito.verify(prdCollectionClient).query(Mockito.any());
  }

  private SolrQuery generateSolrQueryForGroupByCategoryCode(String keyword) {
    SolrQuery solrQuery = new SolrQuery();
    solrQuery = generateSolrQuery(keyword, true);
    solrQuery.set(Constants.GROUP, Boolean.TRUE.toString());
    solrQuery.set(Constants.GROUP + Constants.DOT + Constants.FIELD, "category_code");
    solrQuery.set(Constants.GROUP + Constants.DOT + Constants.N_GROUPS, Boolean.TRUE.toString());
    solrQuery.set(Constants.GROUP + Constants.DOT + Constants.LIMIT, "1");
    solrQuery.setRows(1000);
    return solrQuery;
  }

  private SolrQuery generateSolrQuery(String keyword, boolean categorySearch) {
    SolrQuery solrQuery = new SolrQuery();
    solrQuery.add(CommonParams.Q, keyword);
    solrQuery.add(Constants.DEF_TYPE, Constants.EDISMAX_OPERATION);
    solrQuery.add(Constants.QF, Constants.PRODUCT_NAME_OR_PRODUCT_CODE);
    solrQuery.add(Constants.STOPWORDS, Boolean.TRUE.toString());
    solrQuery.add(Constants.LOWERCASE_OPERATORS, Boolean.TRUE.toString());
    if (!categorySearch) {
      solrQuery.setStart(0);
      solrQuery.setRows(10);
    }
    return solrQuery;
  }

  @Test
  public void getCategoryCodesSolrByKeywordTest() throws Exception {
    Date date = Calendar.getInstance().getTime();
    String docId = UUID.randomUUID().toString();
    SolrDocument solrDocument = getSolrDocument(date, docId);
    SolrDocumentList solrDocumentList = new SolrDocumentList();
    solrDocumentList.add(solrDocument);
    QueryResponse queryResponse = new QueryResponse();
    NamedList<Object> facetValue = new NamedList<>();
    facetValue.add("category1", 1);
    facetValue.add("category3", 2);
    NamedList<Object> facets = new NamedList<>();
    facets.add("facet_fields", facetValue);
    NamedList<Object> facetFields = new NamedList<>();
    facetFields.add("facet_fields", facets);
    NamedList<Object> facetCount = new NamedList<>();
    facetCount.add("facet_counts", facetFields);

    queryResponse.setResponse(facetCount);

    Mockito.when(prdCollectionClient.query(Mockito.any())).thenReturn(queryResponse);
    Set<SolrCategoryCodeDTO> response =
        solrActiveProductCollectionRepositoryBean.getCategoryCodesSolrByKeyword(KEYWORD, 1000,
          null);
    Mockito.verify(prdCollectionClient).query(Mockito.any());
    Assertions.assertEquals(2, response.size());
    Assertions.assertEquals(1, response.iterator().next().getProductCount());
  }

  @Test
  public void getCategoryCodesSolrByKeywordTest_WhenNoFacetsFound() throws Exception {
    Date date = Calendar.getInstance().getTime();
    String docId = UUID.randomUUID().toString();
    SolrDocument solrDocument = getSolrDocument(date, docId);
    SolrDocumentList solrDocumentList = new SolrDocumentList();
    solrDocumentList.add(solrDocument);
    QueryResponse queryResponse = new QueryResponse();

    Mockito.when(prdCollectionClient.query(Mockito.any())).thenReturn(queryResponse);
    Set<SolrCategoryCodeDTO> response =
        solrActiveProductCollectionRepositoryBean.getCategoryCodesSolrByKeyword(KEYWORD, 1000,
          null);
    Mockito.verify(prdCollectionClient).query(Mockito.any());
    Assertions.assertEquals(0, response.size());
  }

  @Test
  public void getCategoryCodesSolrBySpecialCharacterKeywordFound() throws Exception {
    Date date = Calendar.getInstance().getTime();
    String docId = UUID.randomUUID().toString();
    SolrDocument solrDocument = getSolrDocument(date, docId);
    SolrDocumentList solrDocumentList = new SolrDocumentList();
    solrDocumentList.add(solrDocument);
    QueryResponse queryResponse = new QueryResponse();

    Mockito.when(prdCollectionClient.query(Mockito.any())).thenReturn(queryResponse);
    Set<SolrCategoryCodeDTO> response =
        solrActiveProductCollectionRepositoryBean.getCategoryCodesSolrByKeyword(PARENTHESES_OPEN,
          1000, null);
    Mockito.verify(prdCollectionClient).query(Mockito.any());
    Assertions.assertEquals(0, response.size());
  }


  @Test
  public void getCategoryCodesSolrByKeywordB2bExclusiveEnabledWithB2CTest() throws Exception {
    QueryResponse queryResponse = new QueryResponse();
    Mockito.when(prdCollectionClient.query(Mockito.any())).thenReturn(queryResponse);
    ProfileResponse profileResponse = new ProfileResponse();
    List saleChannelList = new ArrayList();
    saleChannelList.add(Constants.B2C_SELLER_CHANNEL);
    CompanyDTO company = new CompanyDTO();
    company.setSalesChannel(saleChannelList);
    profileResponse.setCompany(company);
    Mockito.when(businessPartnerRepository.filterDetailByBusinessPartnerCode(Mockito.anyString())).thenReturn(profileResponse);
    ReflectionTestUtils.setField(solrActiveProductCollectionRepositoryBean, "b2bExclusiveSwitch",
      Boolean.TRUE);
    Set<SolrCategoryCodeDTO> response =
      solrActiveProductCollectionRepositoryBean.getCategoryCodesSolrByKeyword(PARENTHESES_OPEN,
        1000, "B2B_CODE");
    Mockito.verify(prdCollectionClient).query(Mockito.any());
    Assertions.assertEquals(0, response.size());
  }

  @Test
  public void getCategoryCodesSolrByKeywordB2bExclusiveEnabledWithB2BTest() throws Exception {
    QueryResponse queryResponse = new QueryResponse();
    Mockito.when(prdCollectionClient.query(Mockito.any())).thenReturn(queryResponse);
    ProfileResponse profileResponse = new ProfileResponse();
    List saleChannelList = new ArrayList();
    saleChannelList.add(Constants.B2B_SELLER_CHANNEL);
    CompanyDTO company = new CompanyDTO();
    company.setSalesChannel(saleChannelList);
    profileResponse.setCompany(company);
    Mockito.when(businessPartnerRepository.filterDetailByBusinessPartnerCode(Mockito.anyString())).thenReturn(profileResponse);
    ReflectionTestUtils.setField(solrActiveProductCollectionRepositoryBean, "b2bExclusiveSwitch",
      Boolean.TRUE);
    Set<SolrCategoryCodeDTO> response =
      solrActiveProductCollectionRepositoryBean.getCategoryCodesSolrByKeyword(PARENTHESES_OPEN,
        1000, "B2B_CODE");
    Mockito.verify(prdCollectionClient).query(Mockito.any());
    Assertions.assertEquals(0, response.size());
  }

  @Test
  public void getCategoryCodesSolrByKeywordB2bExclusiveEnabledWithB2BWithNoSalesChannelTest() throws Exception {
    QueryResponse queryResponse = new QueryResponse();
    Mockito.when(prdCollectionClient.query(Mockito.any())).thenReturn(queryResponse);
    ProfileResponse profileResponse = new ProfileResponse();
    List saleChannelList = new ArrayList();
    CompanyDTO company = new CompanyDTO();
    company.setSalesChannel(saleChannelList);
    profileResponse.setCompany(company);
    Mockito.when(businessPartnerRepository.filterDetailByBusinessPartnerCode(Mockito.anyString())).thenReturn(profileResponse);
    ReflectionTestUtils.setField(solrActiveProductCollectionRepositoryBean, "b2bExclusiveSwitch",
      Boolean.TRUE);
    Set<SolrCategoryCodeDTO> response =
      solrActiveProductCollectionRepositoryBean.getCategoryCodesSolrByKeyword(PARENTHESES_OPEN,
        1000, "B2B_CODE");
    Mockito.verify(prdCollectionClient).query(Mockito.any());
    Assertions.assertEquals(0, response.size());
  }

  @Test
  public void getCategoryCodesSolrByKeywordB2bExclusiveDisabledTest() throws Exception {
    QueryResponse queryResponse = new QueryResponse();
    Mockito.when(prdCollectionClient.query(Mockito.any())).thenReturn(queryResponse);
    ProfileResponse profileResponse = new ProfileResponse();
    List saleChannelList = new ArrayList();
    saleChannelList.add(Constants.B2B_SELLER_CHANNEL);
    CompanyDTO company = new CompanyDTO();
    company.setSalesChannel(saleChannelList);
    profileResponse.setCompany(company);
    Mockito.when(businessPartnerRepository.filterDetailByBusinessPartnerCode(Mockito.anyString())).thenReturn(profileResponse);
    ReflectionTestUtils.setField(solrActiveProductCollectionRepositoryBean, "b2bExclusiveSwitch",
      Boolean.FALSE);
    Set<SolrCategoryCodeDTO> response =
      solrActiveProductCollectionRepositoryBean.getCategoryCodesSolrByKeyword(PARENTHESES_OPEN,
        1000, "B2B_CODE");
    Mockito.verify(prdCollectionClient).query(Mockito.any());
    Assertions.assertEquals(0, response.size());
  }

  @Test
  public void getCategoryCodesSolrByKeywordB2bExclusiveDisabledWithNoPArtnerProfileTest() throws Exception {
    QueryResponse queryResponse = new QueryResponse();
    Mockito.when(prdCollectionClient.query(Mockito.any())).thenReturn(queryResponse);
    Mockito.when(businessPartnerRepository.filterDetailByBusinessPartnerCode(Mockito.anyString())).thenReturn(null);
    ReflectionTestUtils.setField(solrActiveProductCollectionRepositoryBean, "b2bExclusiveSwitch",
      Boolean.TRUE);
    Set<SolrCategoryCodeDTO> response =
      solrActiveProductCollectionRepositoryBean.getCategoryCodesSolrByKeyword(PARENTHESES_OPEN,
        1000, "B2B_CODE");
    Mockito.verify(prdCollectionClient).query(Mockito.any());
    Assertions.assertEquals(0, response.size());
  }

  @Test
  public void getCategoryCodesSolrByKeywordB2bExclusiveDisabledAndNoPartnerCodeTest() throws Exception {
    QueryResponse queryResponse = new QueryResponse();
    Mockito.when(prdCollectionClient.query(Mockito.any())).thenReturn(queryResponse);
    ProfileResponse profileResponse = new ProfileResponse();
    List saleChannelList = new ArrayList();
    saleChannelList.add(Constants.B2B_SELLER_CHANNEL);
    CompanyDTO company = new CompanyDTO();
    company.setSalesChannel(saleChannelList);
    profileResponse.setCompany(company);
    Mockito.when(businessPartnerRepository.filterDetailByBusinessPartnerCode(Mockito.anyString())).thenReturn(profileResponse);
    ReflectionTestUtils.setField(solrActiveProductCollectionRepositoryBean, "b2bExclusiveSwitch",
      Boolean.FALSE);
    Set<SolrCategoryCodeDTO> response =
      solrActiveProductCollectionRepositoryBean.getCategoryCodesSolrByKeyword(PARENTHESES_OPEN,
        1000, null);
    Mockito.verify(prdCollectionClient).query(Mockito.any());
    Assertions.assertEquals(0, response.size());
  }

  @Test
  public void getCategoryCodesSolrByKeywordB2bExclusiveDisabledAndNoCompanyTest() throws Exception {
    QueryResponse queryResponse = new QueryResponse();
    Mockito.when(prdCollectionClient.query(Mockito.any())).thenReturn(queryResponse);
    ProfileResponse profileResponse = new ProfileResponse();
    profileResponse.setCompany(null);
    Mockito.when(businessPartnerRepository.filterDetailByBusinessPartnerCode(Mockito.anyString())).thenReturn(profileResponse);
    ReflectionTestUtils.setField(solrActiveProductCollectionRepositoryBean, "b2bExclusiveSwitch",
      Boolean.TRUE);
    Set<SolrCategoryCodeDTO> response =
      solrActiveProductCollectionRepositoryBean.getCategoryCodesSolrByKeyword(PARENTHESES_OPEN,
        1000, "B2C");
    Mockito.verify(prdCollectionClient).query(Mockito.any());
    Assertions.assertEquals(0, response.size());
  }


  @Test
  public void getCategoryCodesSolrByKeywordB2bExclusiveEnabledAndNoPartnerCodeTest() throws Exception {
    QueryResponse queryResponse = new QueryResponse();
    Mockito.when(prdCollectionClient.query(Mockito.any())).thenReturn(queryResponse);
    ProfileResponse profileResponse = new ProfileResponse();
    List saleChannelList = new ArrayList();
    saleChannelList.add(Constants.B2B_SELLER_CHANNEL);
    CompanyDTO company = new CompanyDTO();
    company.setSalesChannel(saleChannelList);
    profileResponse.setCompany(company);
    Mockito.when(businessPartnerRepository.filterDetailByBusinessPartnerCode(Mockito.anyString())).thenReturn(profileResponse);
    ReflectionTestUtils.setField(solrActiveProductCollectionRepositoryBean, "b2bExclusiveSwitch",
      Boolean.TRUE);
    Set<SolrCategoryCodeDTO> response =
      solrActiveProductCollectionRepositoryBean.getCategoryCodesSolrByKeyword(PARENTHESES_OPEN,
        1000, null);
    Mockito.verify(prdCollectionClient).query(Mockito.any());
    Assertions.assertEquals(0, response.size());
  }

  @Test
  public void getCategoryCodesSolrByKeyword_ExceptionTest() throws Exception {
    solrActiveProductCollectionRepositoryBean.getCategoryCodesSolrByKeyword("012345", 1000, null);
    Mockito.verify(prdCollectionClient).query(Mockito.any());
  }

  @Test
  public void getProductCodesFromSolrByKeywordAndCategoryCodeTest() throws Exception {
    Date date = Calendar.getInstance().getTime();
    String docId = UUID.randomUUID().toString();
    SolrDocument solrDocument = getSolrDocument(date, docId);
    SolrDocumentList solrDocumentList = new SolrDocumentList();
    solrDocumentList.add(solrDocument);
    QueryResponse queryResponse = new QueryResponse();
    GroupResponse groupResponse = new GroupResponse();
    List<GroupCommand> groupCommands = new ArrayList<>();
    GroupCommand groupCommand = new GroupCommand("product_code", 1, 1);
    groupCommands.add(groupCommand);
    List<Group> groups = new ArrayList<>();
    Group group = new Group("groupValue", solrDocumentList);
    groups.add(group);
    ReflectionTestUtils.setField(groupCommand, GroupCommand.class, "_values", groups, List.class);
    ReflectionTestUtils.setField(groupResponse, GroupResponse.class, "_values", groupCommands, List.class);
    ReflectionTestUtils
        .setField(queryResponse, QueryResponse.class, "_groupResponse", groupResponse, GroupResponse.class);
    ReflectionTestUtils
        .setField(queryResponse, QueryResponse.class, "_results", solrDocumentList, SolrDocumentList.class);
    Mockito.when(prdCollectionClient.query(Mockito.any())).thenReturn(queryResponse);
    solrActiveProductCollectionRepositoryBean
        .getProductCodesFromSolrByKeywordAndCategoryCode("012345", "54321", PageRequest.of(0, 1));
    Mockito.verify(prdCollectionClient).query(Mockito.any());
  }

  private SolrDocument getSolrDocument(Date date, String docId) {
    SolrDocument solrDocument = new SolrDocument();
    solrDocument.addField("id", docId);
    solrDocument.addField("store_id", STORE_ID);
    solrDocument.addField("product_id", PRODUCT_ID);
    solrDocument.addField("product_code", PRODUCT_CODE);
    solrDocument.addField("product_name", PRODUCT_NAME);
    solrDocument.addField("brand", BRAND);
    solrDocument.addField("category_code", CATEGORY_CODE);
    solrDocument.addField("business_partner_code", BP_CODE);
    solrDocument.addField("activated", true);
    solrDocument.addField("viewable", true);
    solrDocument.addField("mark_for_delete", false);
    solrDocument.addField("updated_step_date", date);
    solrDocument.addField("created_date", date);
    solrDocument.addField("created_by", USER_NAME);
    return solrDocument;
  }

  @Test
  public void addSolrProductCollectionDocumentWithB2bExclusiveTrueTest() throws Exception {
    SolrProductCollectionDTO solrProductCollectionDTO =
      new SolrProductCollectionDTO.Builder().setStoreId(STORE_ID).setBrand(BRAND).setProductId(PRODUCT_ID)
        .setBusinessPartnerCode(BP_CODE).setMarkForDelete(false).build();
    Mockito.when(prdCollectionClient.add((SolrInputDocument) Mockito.any())).thenReturn(UPDATE_RESPONSE);
    CategoryResponse categoryResponse = new CategoryResponse();
    categoryResponse.setB2bExclusive(true);
    Mockito.when(productOutbound.getCategoryBasicDetailByCategoryCode(Mockito.any())).thenReturn(categoryResponse);
    solrActiveProductCollectionRepositoryBean.addSolrProductCollectionDocument(solrProductCollectionDTO);
    Mockito.verify(prdCollectionClient).add((SolrInputDocument) Mockito.any());
  }

  @Test
  public void addSolrProductCollectionDocumentWithB2bExclusiveFalseTest() throws Exception {
    SolrProductCollectionDTO solrProductCollectionDTO =
      new SolrProductCollectionDTO.Builder().setStoreId(STORE_ID).setBrand(BRAND).setProductId(PRODUCT_ID)
        .setBusinessPartnerCode(BP_CODE).setMarkForDelete(false).build();
    Mockito.when(prdCollectionClient.add((SolrInputDocument) Mockito.any())).thenReturn(UPDATE_RESPONSE);
    CategoryResponse categoryResponse = new CategoryResponse();
    categoryResponse.setB2bExclusive(false);
    Mockito.when(productOutbound.getCategoryBasicDetailByCategoryCode(Mockito.any())).thenReturn(categoryResponse);
    solrActiveProductCollectionRepositoryBean.addSolrProductCollectionDocument(solrProductCollectionDTO);
    Mockito.verify(prdCollectionClient).add((SolrInputDocument) Mockito.any());
  }

  @Test
  public void addSolrProductCollectionDocumentWithB2bExclusiveFalseWithSwitchDisabledTest() throws Exception {
    SolrProductCollectionDTO solrProductCollectionDTO =
      new SolrProductCollectionDTO.Builder().setStoreId(STORE_ID).setBrand(BRAND).setProductId(PRODUCT_ID)
        .setBusinessPartnerCode(BP_CODE).setMarkForDelete(false).build();
    Mockito.when(prdCollectionClient.add((SolrInputDocument) Mockito.any())).thenReturn(UPDATE_RESPONSE);
    ReflectionTestUtils.setField(solrActiveProductCollectionRepositoryBean, "b2bExclusiveSwitch",
      Boolean.TRUE);
    CategoryResponse categoryResponse = new CategoryResponse();
    categoryResponse.setB2bExclusive(false);
    Mockito.when(productOutbound.getCategoryBasicDetailByCategoryCode(Mockito.any())).thenReturn(categoryResponse);
    solrActiveProductCollectionRepositoryBean.addSolrProductCollectionDocument(solrProductCollectionDTO);
    Mockito.verify(prdCollectionClient).add((SolrInputDocument) Mockito.any());
  }

  @Test
  public void addSolrProductCollectionDocumentTest() throws Exception {
    SolrProductCollectionDTO solrProductCollectionDTO =
        new SolrProductCollectionDTO.Builder().setStoreId(STORE_ID).setBrand(BRAND).setProductId(PRODUCT_ID)
            .setBusinessPartnerCode(BP_CODE).setMarkForDelete(false).build();
    Mockito.when(prdCollectionClient.add((SolrInputDocument) Mockito.any())).thenReturn(UPDATE_RESPONSE);
    solrActiveProductCollectionRepositoryBean.addSolrProductCollectionDocument(solrProductCollectionDTO);
    Mockito.verify(prdCollectionClient).add((SolrInputDocument) Mockito.any());
  }

  @Test
  public void addSolrProductCollectionDocument_ExceptionTest() throws Exception {
    SolrProductCollectionDTO solrProductCollectionDTO = new SolrProductCollectionDTO();
    Mockito.doThrow(IOException.class).when(prdCollectionClient).add((SolrInputDocument) Mockito.any());
    solrActiveProductCollectionRepositoryBean.addSolrProductCollectionDocument(solrProductCollectionDTO);
    Mockito.verify(prdCollectionClient).add((SolrInputDocument) Mockito.any());
  }

  @Test
  public void findProductCodesByKeywordAndCategoryCodesTest() throws Exception {
    QueryResponse queryResponse = new QueryResponse();
    Date date = Calendar.getInstance().getTime();
    String docId = UUID.randomUUID().toString();
    SolrDocument solrDocument = getSolrDocument(date, docId);
    SolrDocumentList solrDocumentList = new SolrDocumentList();
    solrDocumentList.add(solrDocument);
    ReflectionTestUtils
        .setField(queryResponse, QueryResponse.class, "_results", solrDocumentList, SolrDocumentList.class);
    Mockito.when(prdCollectionClient.query(Mockito.any())).thenReturn(queryResponse);
    SolrProductCodeDTO response = solrActiveProductCollectionRepositoryBean
        .findProductCodesByKeywordAndCategoryCodes(KEYWORD, new ArrayList<>(), PageRequest.of(0, 10));
    Mockito.verify(prdCollectionClient).query(Mockito.any());
    Assertions.assertEquals("MTA-012345", PRODUCT_CODE, response.getProductCodes().iterator().next());
  }

  @Test
  public void findProductCodesByKeywordAndCategoryCodesTest_WhenNullQueryResponse() throws Exception {
    Mockito.when(prdCollectionClient.query(Mockito.any())).thenReturn(null);
    SolrProductCodeDTO response = solrActiveProductCollectionRepositoryBean
        .findProductCodesByKeywordAndCategoryCodes(KEYWORD, Arrays.asList("100012", "123123"), PageRequest.of(0, 10));
    Mockito.verify(prdCollectionClient).query(Mockito.any());
  }

  @Test
  public void getAllActiveBrandsByCategoryCodes() throws Exception {
    SolrDocument solrDocument = new SolrDocument();
    solrDocument.addField(SolrFieldNames.BRAND, SolrFieldNames.BRAND);
    SolrDocumentList solrDocumentList = new SolrDocumentList();
    solrDocumentList.add(solrDocument);
    QueryResponse queryResponse = new QueryResponse();
    ReflectionTestUtils
        .setField(queryResponse, QueryResponse.class, "_results", solrDocumentList, SolrDocumentList.class);
    ReflectionTestUtils.setField(queryResponse, QueryResponse.class, "_cursorMarkNext", "cursorMarkNext", String.class);
    Mockito.when(this.prdCollectionClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    List<String> brands = solrActiveProductCollectionRepositoryBean.getAllActiveBrandsByCategoryCodes(categoryCodes);
    Mockito.verify(this.prdCollectionClient, Mockito.times(2)).query(Mockito.any());
    Assertions.assertNotNull(brands);
    Assertions.assertEquals(2, brands.size());
  }

  @Test
  public void getAllActiveBrandsByCategoryCodesWithoutCursorMark() throws Exception {
    SolrDocument solrDocument = new SolrDocument();
    solrDocument.addField(SolrFieldNames.BRAND, SolrFieldNames.BRAND);
    SolrDocumentList solrDocumentList = new SolrDocumentList();
    solrDocumentList.add(solrDocument);
    QueryResponse queryResponse = new QueryResponse();
    ReflectionTestUtils
        .setField(queryResponse, QueryResponse.class, "_results", solrDocumentList, SolrDocumentList.class);
    ReflectionTestUtils.setField(queryResponse, QueryResponse.class, "_cursorMarkNext", "*", String.class);
    Mockito.when(this.prdCollectionClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    List<String> brands = solrActiveProductCollectionRepositoryBean.getAllActiveBrandsByCategoryCodes(categoryCodes);
    Mockito.verify(this.prdCollectionClient).query(Mockito.any());
    Assertions.assertNotNull(brands);
    Assertions.assertEquals(1, brands.size());
  }

  @Test
  public void getAllActiveBrandsByCategoryCodesWithoutCursorMarkSolrException() throws Exception {
    SolrDocument solrDocument = new SolrDocument();
    solrDocument.addField(SolrFieldNames.BRAND, SolrFieldNames.BRAND);
    SolrDocumentList solrDocumentList = new SolrDocumentList();
    solrDocumentList.add(solrDocument);
    QueryResponse queryResponse = new QueryResponse();
    ReflectionTestUtils
        .setField(queryResponse, QueryResponse.class, "_results", solrDocumentList, SolrDocumentList.class);
    ReflectionTestUtils.setField(queryResponse, QueryResponse.class, "_cursorMarkNext", "*", String.class);
    Mockito.when(this.prdCollectionClient.query(Mockito.any(SolrQuery.class))).thenThrow(SolrException.class);
    List<String> brands = solrActiveProductCollectionRepositoryBean.getAllActiveBrandsByCategoryCodes(categoryCodes);
    Mockito.verify(this.prdCollectionClient).query(Mockito.any());
    Assertions.assertNotNull(brands);
    Assertions.assertEquals(0, brands.size());
  }

  @Test
  public void getAllActiveBrandsByCategoryCodesWithoutCursorMarkSolrServerException() throws Exception {
    SolrDocument solrDocument = new SolrDocument();
    solrDocument.addField(SolrFieldNames.BRAND, SolrFieldNames.BRAND);
    SolrDocumentList solrDocumentList = new SolrDocumentList();
    solrDocumentList.add(solrDocument);
    QueryResponse queryResponse = new QueryResponse();
    ReflectionTestUtils
        .setField(queryResponse, QueryResponse.class, "_results", solrDocumentList, SolrDocumentList.class);
    ReflectionTestUtils.setField(queryResponse, QueryResponse.class, "_cursorMarkNext", "*", String.class);
    Mockito.when(this.prdCollectionClient.query(Mockito.any(SolrQuery.class))).thenThrow(SolrServerException.class);
    List<String> brands = solrActiveProductCollectionRepositoryBean.getAllActiveBrandsByCategoryCodes(categoryCodes);
    Mockito.verify(this.prdCollectionClient).query(Mockito.any());
    Assertions.assertNotNull(brands);
    Assertions.assertEquals(0, brands.size());
  }

  @Test
  public void getAllActiveBrandsByCategoryCodesWithoutCursorMarkIOException() throws Exception {
    SolrDocument solrDocument = new SolrDocument();
    solrDocument.addField(SolrFieldNames.BRAND, SolrFieldNames.BRAND);
    SolrDocumentList solrDocumentList = new SolrDocumentList();
    solrDocumentList.add(solrDocument);
    QueryResponse queryResponse = new QueryResponse();
    ReflectionTestUtils
        .setField(queryResponse, QueryResponse.class, "_results", solrDocumentList, SolrDocumentList.class);
    ReflectionTestUtils.setField(queryResponse, QueryResponse.class, "_cursorMarkNext", "*", String.class);
    Mockito.when(this.prdCollectionClient.query(Mockito.any(SolrQuery.class))).thenThrow(IOException.class);
    List<String> brands = solrActiveProductCollectionRepositoryBean.getAllActiveBrandsByCategoryCodes(categoryCodes);
    Mockito.verify(this.prdCollectionClient).query(Mockito.any());
    Assertions.assertNotNull(brands);
    Assertions.assertEquals(0, brands.size());
  }

  @Test
  public void findProductCodesByKeywordFilterQueryTest() throws Exception {
    QueryResponse queryResponse = new QueryResponse();
    Date date = Calendar.getInstance().getTime();
    String docId = UUID.randomUUID().toString();
    SolrDocument solrDocument = getSolrDocument(date, docId);
    SolrDocumentList solrDocumentList = new SolrDocumentList();
    solrDocumentList.add(solrDocument);
    ReflectionTestUtils
        .setField(queryResponse, QueryResponse.class, "_results", solrDocumentList, SolrDocumentList.class);
    Mockito.when(prdCollectionClient.query(Mockito.any())).thenReturn(queryResponse);
    SolrProductCodeDTO response = solrActiveProductCollectionRepositoryBean
        .findProductCodesByKeywordAndCategoryCodes(KEYWORD, new ArrayList<>(), PageRequest.of(0, 10));
    Mockito.verify(prdCollectionClient).query(solrQueryArgumentCaptor.capture());
    Assertions.assertEquals("MTA-012345", PRODUCT_CODE, response.getProductCodes().iterator().next());
    Assertions.assertTrue(solrQueryArgumentCaptor.getValue().get("fq").contains("-review_pending:true"));
  }

  @Test
  public void getCategoryCodesSolrByKeywordTestFilterTest() throws Exception {
    Date date = Calendar.getInstance().getTime();
    String docId = UUID.randomUUID().toString();
    SolrDocument solrDocument = getSolrDocument(date, docId);
    SolrDocumentList solrDocumentList = new SolrDocumentList();
    solrDocumentList.add(solrDocument);
    QueryResponse queryResponse = new QueryResponse();
    Mockito.when(prdCollectionClient.query(Mockito.any())).thenReturn(queryResponse);
    Set<SolrCategoryCodeDTO> response =
        solrActiveProductCollectionRepositoryBean.getCategoryCodesSolrByKeyword(KEYWORD, 1000,
          null);
    Mockito.verify(prdCollectionClient).query(solrQueryArgumentCaptor.capture());
    Assertions.assertTrue(solrQueryArgumentCaptor.getValue().get("fq").contains("-review_pending:true"));
  }

  @Test
  public void getProductCollectionListForFilterRequestTest() throws Exception {
    Date date = Calendar.getInstance().getTime();
    String docId = UUID.randomUUID().toString();
    SolrDocument solrDocument = getSolrDocument(date, docId);
    SolrDocumentList solrDocumentList = new SolrDocumentList();
    solrDocumentList.add(solrDocument);
    QueryResponse queryResponse = new QueryResponse();
    ReflectionTestUtils
        .setField(queryResponse, QueryResponse.class, "_results", solrDocumentList, SolrDocumentList.class);
    queryResponse.getResults().setNumFound(NUM_FOUND);
    Mockito.when(prdCollectionClient.query(Mockito.any())).thenReturn(queryResponse);
    Page<SolrProductCollectionDTO> response = solrActiveProductCollectionRepositoryBean
        .getProductCollectionListForFilterRequest(STORE_ID, productFilterRequest, PageRequest.of(0, 100));
    Mockito.verify(prdCollectionClient).query(solrQueryArgumentCaptor.capture());
    Assertions.assertTrue(solrQueryArgumentCaptor.getValue().get("fq").contains("category_code:\"CD-10230\""));
    Assertions.assertTrue(solrQueryArgumentCaptor.getValue().get("fq").contains("brand:\"apple\""));
    Assertions.assertTrue(solrQueryArgumentCaptor.getValue().get("defType").contains(EDISMAX_PARAMETER));
    Assertions.assertTrue(solrQueryArgumentCaptor.getValue().get("mm").contains("100"));
    Assertions.assertTrue(solrQueryArgumentCaptor.getValue().get("qf").contains(SolrFieldNames.PRODUCT_NAME));
    Assertions.assertEquals(PRODUCT_CODE, response.getContent().get(0).getProductCode());
    Assertions.assertEquals(PRODUCT_NAME, response.getContent().get(0).getProductName());
    Assertions.assertEquals(CATEGORY_CODE, response.getContent().get(0).getCategoryCode());
    Assertions.assertEquals(BRAND, response.getContent().get(0).getBrand());
  }

  @Test
  public void getProductCollectionListForFilterRequestTest_onlyProductNameSearch() throws Exception {
    productFilterRequest.setProductCode(StringUtils.EMPTY);
    Date date = Calendar.getInstance().getTime();
    String docId = UUID.randomUUID().toString();
    SolrDocument solrDocument = getSolrDocument(date, docId);
    SolrDocumentList solrDocumentList = new SolrDocumentList();
    solrDocumentList.add(solrDocument);
    QueryResponse queryResponse = new QueryResponse();
    ReflectionTestUtils
        .setField(queryResponse, QueryResponse.class, "_results", solrDocumentList, SolrDocumentList.class);
    queryResponse.getResults().setNumFound(NUM_FOUND);
    Mockito.when(prdCollectionClient.query(Mockito.any())).thenReturn(queryResponse);
    Page<SolrProductCollectionDTO> response = solrActiveProductCollectionRepositoryBean
        .getProductCollectionListForFilterRequest(STORE_ID, productFilterRequest, PageRequest.of(0, 100));
    Mockito.verify(prdCollectionClient).query(solrQueryArgumentCaptor.capture());
    Assertions.assertTrue(solrQueryArgumentCaptor.getValue().get("fq").contains("category_code:\"CD-10230\""));
    Assertions.assertTrue(solrQueryArgumentCaptor.getValue().get("fq").contains("brand:\"apple\""));
    Assertions.assertTrue(solrQueryArgumentCaptor.getValue().get("defType").contains(EDISMAX_PARAMETER));
    Assertions.assertTrue(solrQueryArgumentCaptor.getValue().get("mm").contains("100"));
    Assertions.assertTrue(solrQueryArgumentCaptor.getValue().get("qf").contains(SolrFieldNames.PRODUCT_NAME));
    Assertions.assertEquals(PRODUCT_CODE, response.getContent().get(0).getProductCode());
    Assertions.assertEquals(PRODUCT_NAME, response.getContent().get(0).getProductName());
    Assertions.assertEquals(CATEGORY_CODE, response.getContent().get(0).getCategoryCode());
    Assertions.assertEquals(BRAND, response.getContent().get(0).getBrand());
  }

  @Test
  public void getProductCollectionListForFilterRequestTest_emptyProductFilters() throws Exception {
    Date date = Calendar.getInstance().getTime();
    String docId = UUID.randomUUID().toString();
    SolrDocument solrDocument = getSolrDocument(date, docId);
    SolrDocumentList solrDocumentList = new SolrDocumentList();
    solrDocumentList.add(solrDocument);
    QueryResponse queryResponse = new QueryResponse();
    ReflectionTestUtils
        .setField(queryResponse, QueryResponse.class, "_results", solrDocumentList, SolrDocumentList.class);
    queryResponse.getResults().setNumFound(NUM_FOUND);
    Mockito.when(prdCollectionClient.query(Mockito.any())).thenReturn(queryResponse);
    Page<SolrProductCollectionDTO> response = solrActiveProductCollectionRepositoryBean
        .getProductCollectionListForFilterRequest(STORE_ID, new ProductFilterRequest(), PageRequest.of(0, 100));
    Mockito.verify(prdCollectionClient).query(solrQueryArgumentCaptor.capture());
    Assertions.assertEquals(PRODUCT_CODE, response.getContent().get(0).getProductCode());
    Assertions.assertEquals(PRODUCT_NAME, response.getContent().get(0).getProductName());
    Assertions.assertEquals(CATEGORY_CODE, response.getContent().get(0).getCategoryCode());
    Assertions.assertEquals(BRAND, response.getContent().get(0).getBrand());
  }

  @Test
  public void deleteSolrProductCollectionByDocumentIdTest() throws SolrServerException, IOException {
    solrActiveProductCollectionRepositoryBean.deleteSolrProductCollectionByDocumentId(PRODUCT_ID);
    Mockito.verify(prdCollectionClient).deleteById(PRODUCT_ID);
  }

  @Test
  public void deleteSolrProductCollectionByDocumentIdExceptionTest() throws SolrServerException, IOException {
    Mockito.doThrow(RuntimeException.class).when(prdCollectionClient).deleteById(PRODUCT_ID);
    solrActiveProductCollectionRepositoryBean.deleteSolrProductCollectionByDocumentId(PRODUCT_ID);
    Mockito.verify(prdCollectionClient).deleteById(PRODUCT_ID);
  }
}
