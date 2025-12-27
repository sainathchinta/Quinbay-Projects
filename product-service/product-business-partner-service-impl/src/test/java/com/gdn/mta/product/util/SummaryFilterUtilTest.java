package com.gdn.mta.product.util;


import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.List;

import org.apache.solr.common.SolrDocument;
import org.apache.solr.common.SolrDocumentList;
import org.apache.solr.common.SolrInputDocument;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.gda.mta.product.dto.ProductBusinessPartnerMapperResponse;
import com.gda.mta.product.dto.response.AssigneeResponse;
import com.gda.mta.product.dto.response.ReviewProductResponse;
import com.gdn.mta.product.entity.UpdatedProductHistory;
import com.gdn.mta.product.entity.ProductCollection;
import com.gdn.mta.product.valueobject.HistorySolr;
import com.gdn.partners.pbp.commons.util.SolrConstants;
import com.gdn.partners.pbp.commons.util.SolrFieldNames;

public class SummaryFilterUtilTest {

  private static final String PRODUCT_SKU = "PRODUCT_SKU";
  private static final String GDN_SKU = "GDN_SKU";
  private static final String GDN_NAME = "GDN_NAME";
  private static final String PICKUP_POINT_CODE = "PICKUP_POINT_CODE";
  private static final String ACTIVITY = "ACTIVITY";
  private static final String ID = "id";

  private static final long TOTAL_NUM_FOUND = 100;

  private SolrDocumentList reviewProductsList;
  private SolrDocumentList businessPartnersList;
  private SolrDocumentList assigneeList;
  private List<ProductCollection> productCollections;
  private SolrDocumentList variantHistoryList;
  private UpdatedProductHistory updatedProductHistory;

  @BeforeEach
  public void setUp() throws Exception {
    reviewProductsList = new SolrDocumentList();
    SolrDocument document = new SolrDocument();
    document.addField(SolrFieldNames.PRODUCT_NAME, SolrFieldNames.PRODUCT_NAME);
    document.addField(SolrFieldNames.PRODUCT_ID, SolrFieldNames.PRODUCT_ID);
    document.addField(SolrFieldNames.PRODUCT_CODE, SolrFieldNames.PRODUCT_CODE);
    document.addField(SolrFieldNames.BUSINESS_PARTNER_NAME, SolrFieldNames.BUSINESS_PARTNER_NAME);
    document.addField(SolrFieldNames.BUSINESS_PARTNER_CODE, SolrFieldNames.BUSINESS_PARTNER_CODE);
    document.addField(SolrFieldNames.CATEGORY_CODES, Arrays.asList(SolrFieldNames.CATEGORY_CODES));
    document.addField(SolrFieldNames.CATEGORY_NAMES, Arrays.asList(SolrFieldNames.CATEGORY_NAMES));
    document.addField(SolrFieldNames.UPDATED_DATE, new Date());
    document.addField(SolrFieldNames.SUBMITTED_DATE, new Date());
    document.addField(SolrFieldNames.CREATED_DATE, new Date());
    document.addField(SolrFieldNames.CREATED_BY, SolrFieldNames.CREATED_BY);
    document.addField(SolrFieldNames.BRAND, SolrFieldNames.BRAND);
    document.addField(SolrFieldNames.ASSIGNED_TO, SolrFieldNames.ASSIGNED_TO);
    document.addField(SolrFieldNames.STATE, SolrFieldNames.STATE);
    document.addField(SolrFieldNames.ID, SolrFieldNames.ID);
    reviewProductsList.add(document);
    reviewProductsList.setNumFound(TOTAL_NUM_FOUND);
    businessPartnersList = new SolrDocumentList();
    SolrDocument solrDocument = new SolrDocument();
    solrDocument.addField(SolrFieldNames.BUSINESS_PARTNER_CODE, SolrFieldNames.BUSINESS_PARTNER_CODE);
    solrDocument.addField(SolrFieldNames.BUSINESS_PARTNER_NAME, SolrFieldNames.BUSINESS_PARTNER_NAME);
    businessPartnersList.add(solrDocument);
    assigneeList = new SolrDocumentList();
    SolrDocument assigneeResponse = new SolrDocument();
    assigneeResponse.addField(SolrFieldNames.ASSIGNED_TO, SolrConstants.ASSIGNED_TO_PREFIX);
    assigneeList.add(assigneeResponse);
    productCollections = new ArrayList<>();
    ProductCollection productCollection = new ProductCollection();
    productCollection.setProductId(SolrFieldNames.PRODUCT_ID);
    productCollection.setProductCode(SolrFieldNames.PRODUCT_CODE);
    productCollection.setBusinessPartnerName(SolrFieldNames.BUSINESS_PARTNER_NAME);
    productCollections.add(productCollection);

    variantHistoryList = new SolrDocumentList();
    SolrDocument variantHistory = new SolrDocument();
    variantHistory.setField(SolrFieldNames.ID, "1");
    variantHistory.setField(SolrFieldNames.ACCESS_TIME, new Date());
    variantHistory.setField(SolrFieldNames.PRODUCT_SKU, SolrFieldNames.PRODUCT_SKU);
    variantHistory.setField(SolrFieldNames.GDN_SKU, SolrFieldNames.GDN_SKU);
    variantHistory.setField(SolrFieldNames.GDN_NAME, SolrFieldNames.GDN_NAME);
    variantHistory.setField(SolrFieldNames.PRODUCT_SKU, SolrFieldNames.PRODUCT_SKU);
    variantHistory.setField(SolrFieldNames.ACTIVITY, SolrFieldNames.ACTIVITY);
    variantHistoryList.add(variantHistory);
    variantHistoryList.add(new SolrDocument());

    updatedProductHistory = new UpdatedProductHistory();
    updatedProductHistory.setAuditTrailId(ID);
    updatedProductHistory.setAccessTime(new Date());
    updatedProductHistory.setGdnSku(GDN_SKU);
    updatedProductHistory.setProductSku(PRODUCT_SKU);
    updatedProductHistory.setGdnName(GDN_NAME);
    updatedProductHistory.setActivity(ACTIVITY);
    updatedProductHistory.setPickupPointCode(PICKUP_POINT_CODE);
    updatedProductHistory.setOnlineStatus(Boolean.TRUE);
  }

  @Test
  public void toReviewProductResponseListTest() {
    List<ReviewProductResponse> responseList = SummaryFilterUtil.toReviewProductResponseList(reviewProductsList);
    Assertions.assertNotNull(responseList);
    Assertions.assertTrue(responseList.size() == 1);
    Assertions.assertTrue(responseList.get(0).getProductName().equals(SolrFieldNames.PRODUCT_NAME));
  }

  @Test
  public void convertQueryResponseToProductBusinessPartnerMapperResponseListTest() {
    List<ProductBusinessPartnerMapperResponse> responseList =
        SummaryFilterUtil.convertQueryResponseToProductBusinessPartnerMapperResponseList(businessPartnersList);
    Assertions.assertNotNull(responseList);
    Assertions.assertTrue(responseList.size() == 1);
    Assertions.assertEquals(responseList.get(0).getBusinessPartnerName(), SolrFieldNames.BUSINESS_PARTNER_NAME);
    Assertions.assertEquals(responseList.get(0).getBusinessPartnerCode(), SolrFieldNames.BUSINESS_PARTNER_CODE);
  }

  @Test
  public void toAssigneeResponseListTest() {
    List<AssigneeResponse> responseList =
        SummaryFilterUtil.toAssigneeResponseList(assigneeList);
    Assertions.assertNotNull(responseList);
    Assertions.assertTrue(responseList.size() == 1);
    Assertions.assertEquals(responseList.get(0).getAssignee(), SolrConstants.ASSIGNED_TO_PREFIX);
  }

  @Test
  public void convertProductCollectionToReviewProductResponseTest() {
    List<ReviewProductResponse> responseList =
        SummaryFilterUtil.convertProductCollectionListToReviewProductResponseList(productCollections);
    Assertions.assertNotNull(responseList);
    Assertions.assertTrue(responseList.size() == 1);
    Assertions.assertEquals(SolrFieldNames.PRODUCT_ID, responseList.get(0).getProductId());
  }

  @Test
  public void convertAssigneeListToAssigneeResponsesTest() {
    List<AssigneeResponse> responseList =
        SummaryFilterUtil.convertAssigneeListToAssigneeResponses(Arrays.asList(SolrFieldNames.ASSIGNED_TO));
    Assertions.assertNotNull(responseList);
    Assertions.assertTrue(responseList.size() == 1);
    Assertions.assertEquals(SolrFieldNames.ASSIGNED_TO, responseList.get(0).getAssignee());
  }

  @Test
  public void convertProductCollectionToProductBusinessPartnerMapperResponseTest() {
    List<ProductBusinessPartnerMapperResponse> responseList =
        SummaryFilterUtil.convertProductCollectionToProductBusinessPartnerMapperResponseList(productCollections);
    Assertions.assertNotNull(responseList);
    Assertions.assertTrue(responseList.size() == 1);
    Assertions.assertEquals(SolrFieldNames.BUSINESS_PARTNER_NAME, responseList.get(0).getBusinessPartnerName());
  }

  @Test
  public void convertToVariantHistorySolrListTest() {
    List<HistorySolr> historySolrList = SummaryFilterUtil.convertToHistorySolrList(variantHistoryList);
    Assertions.assertEquals("1", historySolrList.get(0).getId());
    Assertions.assertEquals(SolrFieldNames.PRODUCT_SKU, historySolrList.get(0).getProductSku());
    Assertions.assertEquals(SolrFieldNames.GDN_SKU, historySolrList.get(0).getGdnSku());
    Assertions.assertEquals(SolrFieldNames.GDN_NAME, historySolrList.get(0).getGdnName());
    Assertions.assertEquals(SolrFieldNames.ACTIVITY, historySolrList.get(0).getActivity());
    Assertions.assertNotNull(historySolrList.get(0).getAccessTime());
    Assertions.assertNull(historySolrList.get(1).getId());
    Assertions.assertNull(historySolrList.get(1).getProductSku());
    Assertions.assertNull(historySolrList.get(1).getGdnSku());
    Assertions.assertNull(historySolrList.get(1).getGdnName());
    Assertions.assertNull(historySolrList.get(1).getActivity());
    Assertions.assertNull(historySolrList.get(1).getAccessTime());
  }

  @Test
  public void convertToVariantHistorySolrListEmptyTest() {
    List<HistorySolr> historySolrList = SummaryFilterUtil.convertToHistorySolrList(new SolrDocumentList());
    Assertions.assertTrue(historySolrList.isEmpty());
  }

  @Test
  public void toSolrInputDocumentsForHistoryTest() {
    List<SolrInputDocument> solrInputDocuments =
        SummaryFilterUtil.toSolrInputDocumentsForHistory(Arrays.asList(updatedProductHistory));
    Assertions.assertEquals(ID, solrInputDocuments.get(0).getFieldValue(SolrFieldNames.ID));
    Assertions.assertEquals(PRODUCT_SKU, solrInputDocuments.get(0).getFieldValue(SolrFieldNames.PRODUCT_SKU));
    Assertions.assertEquals(GDN_SKU, solrInputDocuments.get(0).getFieldValue(SolrFieldNames.GDN_SKU));
    Assertions.assertEquals(GDN_NAME, solrInputDocuments.get(0).getFieldValue(SolrFieldNames.GDN_NAME));
    Assertions.assertEquals(ACTIVITY, solrInputDocuments.get(0).getFieldValue(SolrFieldNames.ACTIVITY));
    Assertions.assertEquals(PICKUP_POINT_CODE, solrInputDocuments.get(0).getFieldValue(SolrFieldNames.PICKUP_POINT_CODE));
    Assertions.assertEquals(Boolean.TRUE, solrInputDocuments.get(0).getFieldValue(SolrFieldNames.ONLINE_STATUS));
    Assertions.assertNotNull(solrInputDocuments.get(0).getFieldValue(SolrFieldNames.ACCESS_TIME));
  }

  @Test
  public void toSolrInputDocumentsForHistoryNullTest() {
    updatedProductHistory = new UpdatedProductHistory();
    updatedProductHistory.setAuditTrailId(ID);
    List<SolrInputDocument> solrInputDocuments =
        SummaryFilterUtil.toSolrInputDocumentsForHistory(Arrays.asList(updatedProductHistory));
    Assertions.assertEquals(ID, solrInputDocuments.get(0).getFieldValue(SolrFieldNames.ID));
    Assertions.assertNull(solrInputDocuments.get(0).getFieldValue(SolrFieldNames.PRODUCT_SKU));
    Assertions.assertNull(solrInputDocuments.get(0).getFieldValue(SolrFieldNames.GDN_SKU));
    Assertions.assertNull(solrInputDocuments.get(0).getFieldValue(SolrFieldNames.GDN_NAME));
    Assertions.assertNull(solrInputDocuments.get(0).getFieldValue(SolrFieldNames.ACTIVITY));
    Assertions.assertNull(solrInputDocuments.get(0).getFieldValue(SolrFieldNames.ACCESS_TIME));
    Assertions.assertNull(solrInputDocuments.get(0).getFieldValue(SolrFieldNames.PICKUP_POINT_CODE));
    Assertions.assertNull(solrInputDocuments.get(0).getFieldValue(SolrFieldNames.ONLINE_STATUS));
  }

}