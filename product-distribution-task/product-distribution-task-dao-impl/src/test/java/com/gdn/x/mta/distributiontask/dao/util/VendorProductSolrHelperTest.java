package com.gdn.x.mta.distributiontask.dao.util;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.StringJoiner;

import com.gdn.x.mta.distributiontask.model.dto.IPRProductListRequest;
import com.gdn.x.mta.distributiontask.model.solr.IprProductSolrFieldNames;
import com.gdn.x.mta.distributiontask.model.enums.ProductStateIPR;
import com.gdn.x.mta.distributiontask.model.solr.IPRProductSolr;
import org.apache.commons.lang3.StringUtils;
import org.apache.solr.client.solrj.SolrQuery;
import org.apache.solr.common.SolrDocument;
import org.apache.solr.common.SolrInputDocument;
import org.apache.solr.common.params.CommonParams;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;

import com.gda.mta.product.dto.ImageQcProcessedAndBrandResponse;
import com.gda.mta.product.dto.ImageQcProcessedResponse;
import com.gdn.x.mta.distributiontask.domain.event.model.PDTProductSolrAddDomainEventModel;
import com.gdn.x.mta.distributiontask.domain.event.model.PDTProductUpdateProductToSolrEventModel;
import com.gdn.x.mta.distributiontask.model.Product;
import com.gdn.x.mta.distributiontask.model.ProductReviewer;
import com.gdn.x.mta.distributiontask.model.Vendor;
import com.gdn.x.mta.distributiontask.model.dto.DistributionTaskMultipleFilterDTO;
import com.gdn.x.mta.distributiontask.model.dto.PrimaryFilterDTO;
import com.gdn.x.mta.distributiontask.model.dto.ProductAndReviewerDetailsDTO;
import com.gdn.x.mta.distributiontask.model.dto.ProductBusinessPartnerMapper;
import com.gdn.x.mta.distributiontask.model.dto.SummaryFilterDTO;
import com.gdn.x.mta.distributiontask.model.enums.SellerBadge;
import com.gdn.x.mta.distributiontask.model.enums.SellerBadgeConstants;
import com.gdn.x.mta.distributiontask.model.enums.SellerType;
import com.gdn.x.mta.distributiontask.model.enums.VendorProductSolrFieldNames;
import com.gdn.x.mta.distributiontask.model.solr.SolrConstants;
import com.gdn.x.mta.distributiontask.model.solr.VendorProductSolr;
import com.gdn.x.mta.distributiontask.model.type.BrandApprovalStatus;
import com.gdn.x.mta.distributiontask.model.type.ProductLabels;
import com.gdn.x.mta.distributiontask.model.type.ProductReviewType;
import com.gdn.x.mta.distributiontask.model.type.ReviewType;
import com.gdn.x.mta.distributiontask.model.type.TimeFilterType;
import com.gdn.x.mta.distributiontask.model.type.WorkflowState;
import com.gdn.x.mta.distributiontask.rest.model.request.ProductListRequest;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;
import com.google.common.collect.ImmutableSet;

public class VendorProductSolrHelperTest {

  private static final String PRODUCT_CODE = "product_code";
  private static final String PRODUCT_NAME = "product_name";
  private static final String CATEGORY_CODE = "category_code";
  private static final String CATEGORY_NAME = "category_name";
  private static final String VENDOR_CODE = "vendorCode";
  private static final String POST_LIVE = "postLive";
  private static final String STORE_ID = "storeId";
  private static final String CREATED_BY = "createdBy";
  private static final String KEYWORD = "keyword";
  private static final String BUSINESS_PARTNER_CODE = "businessPartnerCode";
  private static final String ASSIGNEE_EMAIL = "assigneeEmail";
  private static final String SOURCE = "productSource";
  private static final String GOOD_EN = "Good";
  private static final String NOT_GOOD_EN = "NotGood";
  private static final String ORDER_BY_DESC = "desc";
  private static final String ORDER_BY_ASC = "asc";
  private static final String BRAND_APPROVED_STATE = "APPROVED";
  private static final String BRAND = "brand";
  private static final String REVIEWER_ASSIGNED = "assignedTo";
  private static final String IMAGE = "image";
  private static final String IMAGE_VIOLATION = "blur,good";
  private static final String  MARK_FOR_DELETE = "markForDelete";
  private static final String  STATE = "state";
  private static final String  AND = " AND ";
  private static final String PRODUCT_CREATED_DATE = "[NOW/DAY TO NOW]";
  private static final String BRAND_APPROVAL = "(0 OR 2)";
  private static final String B2B = "B2B";
  private static final String RETAIL = "RETAIL";

  private final Product product = new Product();
  private ProductReviewer productReviewer = new ProductReviewer();
  private final Product product1 = new Product();
  private final ProductReviewer productReviewer1 = new ProductReviewer();
  private final ProductAndReviewerDetailsDTO productAndReviewerDetailsDTO =
    new ProductAndReviewerDetailsDTO(product, productReviewer);
  private final ProductAndReviewerDetailsDTO productAndReviewerDetailsDTO1 =
    new ProductAndReviewerDetailsDTO(product1, productReviewer1);

  private SummaryFilterDTO summaryFilterDTO;
  private final List<WorkflowState> workflowStateList = List.of(WorkflowState.IN_REVIEW);
  private final Pageable pageable = PageRequest.of(0, 10);
  private final SolrDocument vendorSolrInputDocument = new SolrDocument();
  private final PrimaryFilterDTO primaryFilterDTO = new PrimaryFilterDTO();
  private final CategoryResponse categoryResponse = new CategoryResponse();

  private final List<String> pendingStates = new ArrayList<>(Arrays
      .asList(WorkflowState.IN_REVIEW.name(), WorkflowState.EXCEEDED_SLA.name(), WorkflowState.REJECTED.name(),
          WorkflowState.QC_REJECTED.name()));
  private VendorProductSolr vendorProductSolr;
  private ImageQcProcessedAndBrandResponse imageQcProcessedAndBrandResponse;
  private SolrQuery boostQueryForAutoAssignment;
  @BeforeEach
  public void init() {
    product.setState(WorkflowState.IN_REVIEW);
    product.setRejectedCount(Integer.valueOf(1));
    product.setProductCode(PRODUCT_CODE);
    product.setProductName(PRODUCT_NAME);
    product.setCategoryCode(CATEGORY_CODE);
    product.setCategoryName(CATEGORY_NAME);
    product.setCurrentVendor(new Vendor());
    product.getCurrentVendor().setVendorCode(VENDOR_CODE);
    product.setEdited(true);
    product.setReviewType(ReviewType.CONTENT);
    product1.setProductCode(PRODUCT_CODE);
    product1.setProductName(PRODUCT_NAME);
    product1.setCurrentVendor(new Vendor());
    product1.getCurrentVendor().setVendorCode(VENDOR_CODE);
    vendorSolrInputDocument.setField(VendorProductSolrFieldNames.STORE_ID, STORE_ID);
    vendorSolrInputDocument.setField(VendorProductSolrFieldNames.MARK_FOR_DELETE, false);
    vendorSolrInputDocument.setField(VendorProductSolrFieldNames.CREATED_DATE, new Date());
    vendorSolrInputDocument.setField(VendorProductSolrFieldNames.UPDATED_DATE, new Date());
    vendorSolrInputDocument.setField(VendorProductSolrFieldNames.CREATED_BY, CREATED_BY);
    vendorSolrInputDocument.setField(VendorProductSolrFieldNames.PRODUCT_CODE, PRODUCT_CODE);
    vendorSolrInputDocument.setField(VendorProductSolrFieldNames.PRODUCT_NAME, PRODUCT_NAME);
    vendorSolrInputDocument.setField(VendorProductSolrFieldNames.POST_LIVE, false);
    vendorSolrInputDocument
        .setField(VendorProductSolrFieldNames.CATEGORY_CODES, List.of(CATEGORY_CODE));
    vendorSolrInputDocument
        .setField(VendorProductSolrFieldNames.CATEGORY_NAMES, List.of(CATEGORY_NAME));
    vendorSolrInputDocument.setField(VendorProductSolrFieldNames.BRAND, BRAND);
    vendorSolrInputDocument
        .setField(VendorProductSolrFieldNames.BUSINESS_PARTNER_CODE, BUSINESS_PARTNER_CODE);
    vendorSolrInputDocument
        .setField(VendorProductSolrFieldNames.BUSINESS_PARTNER_NAME, BUSINESS_PARTNER_CODE);
    vendorSolrInputDocument
        .setField(VendorProductSolrFieldNames.PRODUCT_CREATED_DATE, new Date());
    vendorSolrInputDocument
        .setField(VendorProductSolrFieldNames.REJECTED_COUNT, 0);
    vendorSolrInputDocument
        .setField(VendorProductSolrFieldNames.STATE, WorkflowState.IN_REVIEW.toString());
    vendorSolrInputDocument
        .setField(VendorProductSolrFieldNames.VENDOR_CODE, VENDOR_CODE);
    vendorSolrInputDocument
        .setField(VendorProductSolrFieldNames.BRAND_APPROVAL_STATUS, BrandApprovalStatus.APPROVED.getValue());
    vendorSolrInputDocument
      .setField(VendorProductSolrFieldNames.PRODUCT_REVIEW_TYPE, ProductReviewType.NEWLY_ADDED.getValue());
    vendorSolrInputDocument
        .setField(VendorProductSolrFieldNames.PRODUCT_PREDICTION_SCORE, 0);
    vendorSolrInputDocument
        .setField(VendorProductSolrFieldNames.IMAGE_VIOLATIONS, List.of(GOOD_EN));
    vendorSolrInputDocument.setField(VendorProductSolrFieldNames.QC_RETRY_COUNT, 0);
    vendorSolrInputDocument.setField(VendorProductSolrFieldNames.APPROVED_DATE, new Date());
    vendorSolrInputDocument.setField(VendorProductSolrFieldNames.ASSIGNED_DATE, new Date());
    vendorSolrInputDocument.setField(VendorProductSolrFieldNames.APPROVER_ASSIGNEE, ASSIGNEE_EMAIL);
    vendorSolrInputDocument.setField(VendorProductSolrFieldNames.REVISED, true);
    vendorSolrInputDocument.setField(VendorProductSolrFieldNames.RESTRICTED_KEYWORDS_PRESENT, true);
    vendorSolrInputDocument.setField(VendorProductSolrFieldNames.REVIEW_TYPE, 3);
    vendorSolrInputDocument.setField(VendorProductSolrFieldNames.PRODUCT_CHANNEL, new ArrayList());
    vendorSolrInputDocument.setField(VendorProductSolrFieldNames.APPEALED_PRODUCT, true);
    vendorSolrInputDocument.setField(VendorProductSolrFieldNames.DISTRIBUTION_MAPPING_STATUS, 1);
    vendorSolrInputDocument.setField(VendorProductSolrFieldNames.PRODUCT_CREATION_TYPE, "PRODUCT_CREATION_TYPE");

    categoryResponse.setCategoryCode(CATEGORY_CODE);
    categoryResponse.setName(CATEGORY_NAME);

    productReviewer = ProductReviewer.builder().approverAssignee(IMAGE).productCode(PRODUCT_CODE).build();
    vendorProductSolr = new VendorProductSolr();
    vendorProductSolr.setProductCode(PRODUCT_CODE);
    vendorProductSolr.setProductApproverAssignee(SolrConstants.NOT_APPLICABLE);
    vendorProductSolr.setCategoryCodes(new ArrayList<>(List.of(CATEGORY_CODE)));
    vendorProductSolr.setCategoryNames(new ArrayList<>(List.of(CATEGORY_CODE)));
    vendorProductSolr.setImageViolations(SolrConstants.NOT_APPLICABLE);
    productReviewer.setApproverAssignee(ASSIGNEE_EMAIL);
    productReviewer.setAssignedDate(new Date());
    productReviewer.setApprovedDate(new Date());
    imageQcProcessedAndBrandResponse = new ImageQcProcessedAndBrandResponse();
    boostQueryForAutoAssignment = new SolrQuery();
    summaryFilterDTO = new SummaryFilterDTO();
  }

  @Test
   void setProductInfoTest() {
    ProductAndReviewerDetailsDTO response = VendorProductSolrHelper.toProductAndReviewerDetailsDTO(vendorProductSolr);
    Assertions.assertNull(response.getProductReviewer().getApproverAssignee());
    Assertions.assertNull(response.getProduct().getImageViolations());
  }

  @Test
   void setProductInfoApproverAssigneeApplicableTest() {
    vendorProductSolr.setProductApproverAssignee(PRODUCT_NAME);
    ProductAndReviewerDetailsDTO response = VendorProductSolrHelper.toProductAndReviewerDetailsDTO(vendorProductSolr);
    Assertions.assertEquals(PRODUCT_NAME, response.getProductReviewer().getApproverAssignee());
    Assertions.assertNull(response.getProduct().getImageViolations());
  }

  @Test
   void getQueryForDeleteTest() {
    String result = VendorProductSolrHelper.getQueryForDeleteAll();
    Assertions.assertEquals(SolrConstants.QUERY_PRODUCT_CODE + SolrConstants.LIKE_QUERY, result);
  }

  @Test
   void toPDTProductSolrAddDomainEventModelListTest() throws Exception {
    Map<String, ProductReviewer> productReviewerMap = new HashMap<>();
    List<String> productChannel = new ArrayList<>();
    productChannel.add(RETAIL);
    productReviewerMap.put(PRODUCT_CODE, ProductReviewer.builder().productCode(PRODUCT_CODE).build());
    productAndReviewerDetailsDTO.getProduct().setPredictedBrand(BRAND);
    productAndReviewerDetailsDTO.getProduct().setTextViolations(ProductLabels.GOOGLE_RESTRICTION.name());
    productAndReviewerDetailsDTO.getProduct().setForceReview(Boolean.TRUE);
    productAndReviewerDetailsDTO.getProduct().setPostLive(Boolean.TRUE);
    productAndReviewerDetailsDTO.getProduct().setSellerType(SellerType.NON_TRUSTED_SELLER);
    productAndReviewerDetailsDTO.getProduct().setSellerBadge(
      SellerBadge.GOLD_MERCHANT);
    productAndReviewerDetailsDTO.getProduct().setB2bActivated(false);
    productAndReviewerDetailsDTO.getProduct().setB2cActivated(true);
    List<PDTProductSolrAddDomainEventModel> productSolrAddDomainEventModelList =
      VendorProductSolrHelper.toPDTProductSolrAddDomainEventModelList(
        Arrays.asList(productAndReviewerDetailsDTO, productAndReviewerDetailsDTO1));
    Assertions.assertEquals(PRODUCT_CODE,
        productSolrAddDomainEventModelList.get(0).getProductCode());
    Assertions.assertEquals(PRODUCT_NAME,
        productSolrAddDomainEventModelList.get(0).getProductName());
    Assertions.assertEquals(WorkflowState.IN_REVIEW.toString(),
        productSolrAddDomainEventModelList.get(0).getState());
    Assertions.assertEquals(1, productSolrAddDomainEventModelList.get(0).getRejectedCount());
    Assertions.assertEquals(StringUtils.EMPTY,
        productSolrAddDomainEventModelList.get(1).getState());
    Assertions.assertEquals(0, productSolrAddDomainEventModelList.get(1).getRejectedCount());
    Assertions.assertEquals(CATEGORY_CODE,
        productSolrAddDomainEventModelList.get(0).getCategoryCodes().get(0));
    Assertions.assertEquals(CATEGORY_NAME,
        productSolrAddDomainEventModelList.get(0).getCategoryNames().get(0));
    Assertions.assertEquals(ReviewType.CONTENT.name(),
        productSolrAddDomainEventModelList.get(0).getReviewType());
    Assertions.assertTrue(
        StringUtils.isEmpty(productSolrAddDomainEventModelList.get(1).getReviewType()));
    Assertions.assertEquals(BRAND, productSolrAddDomainEventModelList.get(0).getPredictedBrand());
    Assertions.assertTrue(productSolrAddDomainEventModelList.get(0)
        .getImageViolations().contains(ProductLabels.GOOGLE_RESTRICTION.name()));
    Assertions.assertFalse(productSolrAddDomainEventModelList.get(0).isPostLive());
    Assertions.assertEquals(SellerBadgeConstants.GOLD_MERCHANT.name(),
        productSolrAddDomainEventModelList.get(0).getSellerBadge());
    Assertions.assertEquals(productChannel,
        productSolrAddDomainEventModelList.get(0).getProductChannel());
  }

  @Test
   void toPDTProductSolrAddDomainEventModelList_withForceReviewFalseTest() throws Exception {
    Map<String, ProductReviewer> productReviewerMap = new HashMap<>();
    List<String> productChannel = new ArrayList<>();
    productChannel.add(B2B);
    productReviewerMap.put(PRODUCT_CODE, ProductReviewer.builder().productCode(PRODUCT_CODE).build());
    productAndReviewerDetailsDTO.getProduct().setPredictedBrand(BRAND);
    productAndReviewerDetailsDTO.getProduct().setTextViolations(ProductLabels.GOOGLE_RESTRICTION.name());
    productAndReviewerDetailsDTO.getProduct().setForceReview(Boolean.FALSE);
    productAndReviewerDetailsDTO.getProduct().setPostLive(Boolean.TRUE);
    productAndReviewerDetailsDTO.getProduct().setSellerType(SellerType.NON_TRUSTED_SELLER);
    productAndReviewerDetailsDTO.getProduct().setSellerBadge(
     SellerBadge.BRONZE_MERCHANT);
    productAndReviewerDetailsDTO.getProduct().setB2cActivated(Boolean.FALSE);
    productAndReviewerDetailsDTO.getProduct().setB2bActivated(Boolean.TRUE);
    productAndReviewerDetailsDTO.getProduct().setDistributionMappingStatus(1);
    List<PDTProductSolrAddDomainEventModel> productSolrAddDomainEventModelList =
      VendorProductSolrHelper.toPDTProductSolrAddDomainEventModelList(
        Arrays.asList(productAndReviewerDetailsDTO, productAndReviewerDetailsDTO1));
    Assertions.assertEquals(PRODUCT_CODE,
        productSolrAddDomainEventModelList.get(0).getProductCode());
    Assertions.assertEquals(PRODUCT_NAME,
        productSolrAddDomainEventModelList.get(0).getProductName());
    Assertions.assertEquals(WorkflowState.IN_REVIEW.toString(),
        productSolrAddDomainEventModelList.get(0).getState());
    Assertions.assertEquals(1, productSolrAddDomainEventModelList.get(0).getRejectedCount());
    Assertions.assertEquals(StringUtils.EMPTY,
        productSolrAddDomainEventModelList.get(1).getState());
    Assertions.assertEquals(0, productSolrAddDomainEventModelList.get(1).getRejectedCount());
    Assertions.assertEquals(CATEGORY_CODE,
        productSolrAddDomainEventModelList.get(0).getCategoryCodes().get(0));
    Assertions.assertEquals(CATEGORY_NAME,
        productSolrAddDomainEventModelList.get(0).getCategoryNames().get(0));
    Assertions.assertEquals(ReviewType.CONTENT.name(),
        productSolrAddDomainEventModelList.get(0).getReviewType());
    Assertions.assertTrue(
        StringUtils.isEmpty(productSolrAddDomainEventModelList.get(1).getReviewType()));
    Assertions.assertEquals(BRAND, productSolrAddDomainEventModelList.get(0).getPredictedBrand());
    Assertions.assertTrue(productSolrAddDomainEventModelList.get(0)
      .getImageViolations().contains(ProductLabels.GOOGLE_RESTRICTION.name()));
    Assertions.assertTrue(productSolrAddDomainEventModelList.get(0).isPostLive());
    Assertions.assertEquals(SellerBadge.BRONZE_MERCHANT.name(),
        productSolrAddDomainEventModelList.get(0).getSellerBadge());
    Assertions.assertEquals(productChannel,
        productSolrAddDomainEventModelList.get(0).getProductChannel());
    Assertions.assertEquals(1,productSolrAddDomainEventModelList.get(0).getDistributionMappingStatus());
  }

  @Test
   void toPDTProductSolrAddDomainEventModelList_forTrustedSellersTest() throws Exception {
    Map<String, ProductReviewer> productReviewerMap = new HashMap<>();
    List<String> productChannel = new ArrayList<>();
    productChannel.add(B2B);
    productChannel.add(RETAIL);
    productReviewerMap.put(PRODUCT_CODE, ProductReviewer.builder().productCode(PRODUCT_CODE).build());
    productAndReviewerDetailsDTO.getProduct().setPredictedBrand(BRAND);
    productAndReviewerDetailsDTO.getProduct().setTextViolations(ProductLabels.GOOGLE_RESTRICTION.name());
    productAndReviewerDetailsDTO.getProduct().setForceReview(Boolean.TRUE);
    productAndReviewerDetailsDTO.getProduct().setPostLive(Boolean.TRUE);
    productAndReviewerDetailsDTO.getProduct().setSellerType(SellerType.TRUSTED_SELLER);
    productAndReviewerDetailsDTO.getProduct().setB2cActivated(Boolean.TRUE);
    productAndReviewerDetailsDTO.getProduct().setB2bActivated(Boolean.TRUE);
    List<PDTProductSolrAddDomainEventModel> productSolrAddDomainEventModelList =
      VendorProductSolrHelper.toPDTProductSolrAddDomainEventModelList(
        Arrays.asList(productAndReviewerDetailsDTO, productAndReviewerDetailsDTO1));
    Assertions.assertEquals(PRODUCT_CODE,
        productSolrAddDomainEventModelList.get(0).getProductCode());
    Assertions.assertEquals(PRODUCT_NAME,
        productSolrAddDomainEventModelList.get(0).getProductName());
    Assertions.assertEquals(WorkflowState.IN_REVIEW.toString(),
        productSolrAddDomainEventModelList.get(0).getState());
    Assertions.assertEquals(1, productSolrAddDomainEventModelList.get(0).getRejectedCount());
    Assertions.assertEquals(StringUtils.EMPTY,
        productSolrAddDomainEventModelList.get(1).getState());
    Assertions.assertEquals(0, productSolrAddDomainEventModelList.get(1).getRejectedCount());
    Assertions.assertEquals(CATEGORY_CODE,
        productSolrAddDomainEventModelList.get(0).getCategoryCodes().get(0));
    Assertions.assertEquals(CATEGORY_NAME,
        productSolrAddDomainEventModelList.get(0).getCategoryNames().get(0));
    Assertions.assertEquals(ReviewType.CONTENT.name(),
        productSolrAddDomainEventModelList.get(0).getReviewType());
    Assertions.assertTrue(
        StringUtils.isEmpty(productSolrAddDomainEventModelList.get(1).getReviewType()));
    Assertions.assertEquals(BRAND, productSolrAddDomainEventModelList.get(0).getPredictedBrand());
    Assertions.assertTrue(productSolrAddDomainEventModelList.get(0)
      .getImageViolations().contains(ProductLabels.GOOGLE_RESTRICTION.name()));
    Assertions.assertEquals(productChannel,
        productSolrAddDomainEventModelList.get(0).getProductChannel());
  }

  @Test
   void toPDTProductSolrAddDomainEventModelList_withForceReviewFalseForTrustedSellersTest() throws Exception {
    Map<String, ProductReviewer> productReviewerMap = new HashMap<>();
    productReviewerMap.put(PRODUCT_CODE, ProductReviewer.builder().productCode(PRODUCT_CODE).build());
    productAndReviewerDetailsDTO.getProduct().setPredictedBrand(BRAND);
    productAndReviewerDetailsDTO.getProduct().setTextViolations(ProductLabels.GOOGLE_RESTRICTION.name());
    productAndReviewerDetailsDTO.getProduct().setForceReview(Boolean.FALSE);
    productAndReviewerDetailsDTO.getProduct().setPostLive(Boolean.TRUE);
    productAndReviewerDetailsDTO.getProduct().setSellerType(SellerType.TRUSTED_SELLER);
    List<PDTProductSolrAddDomainEventModel> productSolrAddDomainEventModelList =
      VendorProductSolrHelper.toPDTProductSolrAddDomainEventModelList(
        Arrays.asList(productAndReviewerDetailsDTO, productAndReviewerDetailsDTO1));
    Assertions.assertEquals(PRODUCT_CODE,
        productSolrAddDomainEventModelList.get(0).getProductCode());
    Assertions.assertEquals(PRODUCT_NAME,
        productSolrAddDomainEventModelList.get(0).getProductName());
    Assertions.assertEquals(WorkflowState.IN_REVIEW.toString(),
        productSolrAddDomainEventModelList.get(0).getState());
    Assertions.assertEquals(1, productSolrAddDomainEventModelList.get(0).getRejectedCount());
    Assertions.assertEquals(StringUtils.EMPTY,
        productSolrAddDomainEventModelList.get(1).getState());
    Assertions.assertEquals(0, productSolrAddDomainEventModelList.get(1).getRejectedCount());
    Assertions.assertEquals(CATEGORY_CODE,
        productSolrAddDomainEventModelList.get(0).getCategoryCodes().get(0));
    Assertions.assertEquals(CATEGORY_NAME,
        productSolrAddDomainEventModelList.get(0).getCategoryNames().get(0));
    Assertions.assertEquals(ReviewType.CONTENT.name(),
        productSolrAddDomainEventModelList.get(0).getReviewType());
    Assertions.assertTrue(
        StringUtils.isEmpty(productSolrAddDomainEventModelList.get(1).getReviewType()));
    Assertions.assertEquals(BRAND, productSolrAddDomainEventModelList.get(0).getPredictedBrand());
    Assertions.assertTrue(productSolrAddDomainEventModelList.get(0)
      .getImageViolations().contains(ProductLabels.GOOGLE_RESTRICTION.name()));
    Assertions.assertTrue(productSolrAddDomainEventModelList.get(0).isPostLive());
  }

  @Test
   void getSolrQueryForVendorListTest() {
    summaryFilterDTO =
        SummaryFilterDTO.builder().timeFilterType(TimeFilterType.ALL).vendorCode(VENDOR_CODE)
            .build();
    SolrQuery solrQuery = VendorProductSolrHelper
        .getSolrQueryForVendorList(STORE_ID, summaryFilterDTO, workflowStateList, pageable);
    Assertions.assertNotNull(solrQuery.getQuery());
  }

  @Test
   void getSolrQueryForVendorList_restrictedKeywordTest() {
    summaryFilterDTO =
      SummaryFilterDTO.builder().restrictedKeyword(Boolean.TRUE).vendorCode(VENDOR_CODE)
        .build();
    SolrQuery solrQuery = VendorProductSolrHelper
      .getSolrQueryForVendorList(STORE_ID, summaryFilterDTO, workflowStateList, pageable);
    Assertions.assertNotNull(solrQuery.getQuery());
  }

  @Test
   void getSolrQueryForVendorList_appealedProductTest() {
    summaryFilterDTO =
        SummaryFilterDTO.builder().appealedProduct(Boolean.TRUE).vendorCode(VENDOR_CODE)
            .build();
    SolrQuery solrQuery = VendorProductSolrHelper
        .getSolrQueryForVendorList(STORE_ID, summaryFilterDTO, workflowStateList, pageable);
    Assertions.assertNotNull(solrQuery.getQuery());
  }

  @Test
   void getSolrQueryForVendorListTest_withKeyword() {
    summaryFilterDTO =
        SummaryFilterDTO.builder().timeFilterType(TimeFilterType.ALL).vendorCode(VENDOR_CODE)
            .keyword(KEYWORD).b2bActivated(true).b2cActivated(true).build();
    SolrQuery solrQuery = VendorProductSolrHelper
        .getSolrQueryForVendorList(STORE_ID, summaryFilterDTO, workflowStateList, pageable);
    Assertions.assertTrue(solrQuery.getQuery().contains(KEYWORD));
    Assertions.assertTrue(
        Arrays.stream(solrQuery.getFilterQueries()).anyMatch(query -> query.equals("productChannel:(RETAIL AND B2B)")));
  }


  @Test
   void getSolrQueryForVendorListTest_emptyList() {
    summaryFilterDTO =
        SummaryFilterDTO.builder().timeFilterType(TimeFilterType.ALL).b2bActivated(true).vendorCode(VENDOR_CODE)
            .build();
    SolrQuery solrQuery = VendorProductSolrHelper
        .getSolrQueryForVendorList(STORE_ID, summaryFilterDTO, new ArrayList<>(), pageable);
    Assertions.assertFalse(solrQuery.getQuery().contains(workflowStateList.toString()));
  }

  @Test
   void getSolrQueryForVendorListTest_timeFilterTypeFiveDays() {
    summaryFilterDTO = SummaryFilterDTO.builder().b2cActivated(true).timeFilterType(TimeFilterType.FIVE_DAYS_AGO)
        .vendorCode(VENDOR_CODE).build();
    SolrQuery solrQuery = VendorProductSolrHelper
        .getSolrQueryForVendorList(STORE_ID, summaryFilterDTO, new ArrayList<>(), pageable);
    solrQuery.getQuery().contains(SolrConstants.FIVE_DAYS_AGO_FACET_INTERVAL
        .replace(SolrConstants.COMMA, StringUtils.SPACE + SolrConstants.TO));
  }

  @Test
   void getSolrQueryForVendorListTest_timeFilterTypeThreeToFiveDays() {
    summaryFilterDTO =
        SummaryFilterDTO.builder().timeFilterType(TimeFilterType.THREE_TO_FIVE_DAYS_AGO)
            .vendorCode(VENDOR_CODE).build();
    SolrQuery solrQuery = VendorProductSolrHelper
        .getSolrQueryForVendorList(STORE_ID, summaryFilterDTO, new ArrayList<>(), pageable);
    solrQuery.getQuery().contains(SolrConstants.THREE_TO_FIVE_DAYS_AGO_FACET_INTERVAL
        .replace(SolrConstants.COMMA, StringUtils.SPACE + SolrConstants.TO));
  }

  @Test
   void getSolrQueryForVendorListTest_timeFilterTypeTwoDays() {
    summaryFilterDTO =
        SummaryFilterDTO.builder().timeFilterType(TimeFilterType.TWO_DAYS_AGO)
            .vendorCode(VENDOR_CODE).build();
    SolrQuery solrQuery = VendorProductSolrHelper
        .getSolrQueryForVendorList(STORE_ID, summaryFilterDTO, new ArrayList<>(), pageable);
    solrQuery.getQuery().contains(SolrConstants.TWO_DAYS_AGO_FACET_INTERVAL
        .replace(SolrConstants.COMMA, StringUtils.SPACE + SolrConstants.TO));
  }

  @Test
   void getSolrQueryForVendorListTest_timeFilterTypeYesterday() {
    summaryFilterDTO =
        SummaryFilterDTO.builder().timeFilterType(TimeFilterType.YESTERDAY)
            .vendorCode(VENDOR_CODE).build();
    SolrQuery solrQuery = VendorProductSolrHelper
        .getSolrQueryForVendorList(STORE_ID, summaryFilterDTO, new ArrayList<>(), pageable);
    solrQuery.getQuery().contains(SolrConstants.YESTERDAY_FACET_INTERVAL
        .replace(SolrConstants.COMMA, StringUtils.SPACE + SolrConstants.TO));
  }

  @Test
   void getSolrQueryForVendorListTest_timeFilterTypeToday() {
    summaryFilterDTO =
        SummaryFilterDTO.builder().timeFilterType(TimeFilterType.TODAY)
            .vendorCode(VENDOR_CODE).build();
    SolrQuery solrQuery = VendorProductSolrHelper
        .getSolrQueryForVendorList(STORE_ID, summaryFilterDTO, new ArrayList<>(), pageable);
    solrQuery.getQuery().contains(SolrConstants.TODAY_FACET_INTERVAL
        .replace(SolrConstants.COMMA, StringUtils.SPACE + SolrConstants.TO));
  }

  @Test
   void getSolrQueryForVendorListTest_nullTimeFilterType() {
    summaryFilterDTO = SummaryFilterDTO.builder().vendorCode(VENDOR_CODE).build();
    SolrQuery solrQuery = VendorProductSolrHelper
        .getSolrQueryForVendorList(STORE_ID, summaryFilterDTO, new ArrayList<>(), pageable);
    Assertions.assertNotNull(solrQuery.getQuery());
  }

  @Test
   void getSolrQueryForVendorListTest_categoryCode() {
    summaryFilterDTO =
        SummaryFilterDTO.builder().timeFilterType(TimeFilterType.ALL).categoryCode(CATEGORY_CODE)
            .vendorCode(VENDOR_CODE).build();
    SolrQuery solrQuery = VendorProductSolrHelper
        .getSolrQueryForVendorList(STORE_ID, summaryFilterDTO, new ArrayList<>(), pageable);
    Assertions.assertTrue(solrQuery.getQuery().contains(CATEGORY_CODE));
  }

  @Test
   void getSolrQueryForVendorListTest_allExternalMerchants() {
    summaryFilterDTO = SummaryFilterDTO.builder().timeFilterType(TimeFilterType.ALL)
        .businessPartnerCode(SolrConstants.EXTERNAL).vendorCode(VENDOR_CODE).build();
    SolrQuery solrQuery = VendorProductSolrHelper
        .getSolrQueryForVendorList(STORE_ID, summaryFilterDTO, new ArrayList<>(), pageable);
    Assertions.assertTrue(solrQuery.getQuery().contains(SolrConstants.INTERNAL));
  }

  @Test
   void getSolrQueryForVendorListTest_singleMerchant() {
    summaryFilterDTO = SummaryFilterDTO.builder().timeFilterType(TimeFilterType.ALL)
        .businessPartnerCode(BUSINESS_PARTNER_CODE).vendorCode(VENDOR_CODE).build();
    SolrQuery solrQuery = VendorProductSolrHelper
        .getSolrQueryForVendorList(STORE_ID, summaryFilterDTO, new ArrayList<>(), pageable);
    Assertions.assertTrue(solrQuery.getQuery().contains(BUSINESS_PARTNER_CODE));
  }

  @Test
   void getSolrQueryForVendorListTest_assigneeEmailId() {
    summaryFilterDTO = SummaryFilterDTO.builder().timeFilterType(TimeFilterType.ALL)
        .assigneeEmailId(ASSIGNEE_EMAIL).vendorCode(VENDOR_CODE).build();
    SolrQuery solrQuery = VendorProductSolrHelper
        .getSolrQueryForVendorList(STORE_ID, summaryFilterDTO, new ArrayList<>(), pageable);
    Assertions.assertTrue(solrQuery.getQuery().contains(ASSIGNEE_EMAIL));
  }

  @Test
   void getSolrQueryForVendorListTest_emptyVendorCode() {
    summaryFilterDTO = SummaryFilterDTO.builder().timeFilterType(TimeFilterType.ALL).build();
    SolrQuery solrQuery = VendorProductSolrHelper
        .getSolrQueryForVendorList(STORE_ID, summaryFilterDTO, new ArrayList<>(), pageable);
    Assertions.assertFalse(solrQuery.getQuery().contains(VENDOR_CODE));
  }

  @Test
   void getSolrQueryForVendorListTest_brandPendingTrue() {
    summaryFilterDTO = SummaryFilterDTO.builder().timeFilterType(TimeFilterType.ALL)
        .brandPending(Boolean.TRUE).vendorCode(VENDOR_CODE).build();
    SolrQuery solrQuery = VendorProductSolrHelper
        .getSolrQueryForVendorList(STORE_ID, summaryFilterDTO, new ArrayList<>(), pageable);
    Assertions.assertTrue(
        solrQuery.getFilterQueries()[3].contains(BrandApprovalStatus.APPROVED.getValue()));
  }

  @Test
   void getSolrQueryForVendorListTest_brandPendingFalse() {
    summaryFilterDTO = SummaryFilterDTO.builder().timeFilterType(TimeFilterType.ALL)
        .brandPending(Boolean.FALSE).vendorCode(VENDOR_CODE).build();
    SolrQuery solrQuery = VendorProductSolrHelper
        .getSolrQueryForVendorList(STORE_ID, summaryFilterDTO, new ArrayList<>(), pageable);
    Assertions.assertFalse(solrQuery.getQuery().contains(BRAND_APPROVED_STATE));
  }

  @Test
   void getSolrQueryForVendorListTest_assignmentFalse() {
    summaryFilterDTO = SummaryFilterDTO.builder().timeFilterType(TimeFilterType.ALL)
        .assignment(Boolean.FALSE).vendorCode(VENDOR_CODE).build();
    SolrQuery solrQuery = VendorProductSolrHelper
        .getSolrQueryForVendorList(STORE_ID, summaryFilterDTO, new ArrayList<>(), pageable);
    Assertions.assertNotNull(solrQuery.getQuery());
  }

  @Test
   void getSolrQueryForVendorListTest_assignmentTrue() {
    summaryFilterDTO = SummaryFilterDTO.builder().timeFilterType(TimeFilterType.ALL)
        .assignment(Boolean.TRUE).vendorCode(VENDOR_CODE).build();
    SolrQuery solrQuery = VendorProductSolrHelper
        .getSolrQueryForVendorList(STORE_ID, summaryFilterDTO, new ArrayList<>(), pageable);
    Assertions.assertNotNull(solrQuery.getQuery());
  }

  @Test
   void getSolrQueryForVendorListTest_faultyImageTypeGood() {
    summaryFilterDTO = SummaryFilterDTO.builder().timeFilterType(TimeFilterType.ALL)
        .faultyImageType(ProductLabels.GOOD.getDescription()).vendorCode(VENDOR_CODE).build();
    SolrQuery solrQuery = VendorProductSolrHelper
        .getSolrQueryForVendorList(STORE_ID, summaryFilterDTO, new ArrayList<>(), pageable);
    Assertions.assertTrue(solrQuery.getQuery().contains(SolrConstants.NOT_APPLICABLE));
  }

  @Test
   void getSolrQueryForVendorListTest_faultyImageTypeNotGood() {
    summaryFilterDTO = SummaryFilterDTO.builder().timeFilterType(TimeFilterType.ALL)
        .faultyImageType(NOT_GOOD_EN).vendorCode(VENDOR_CODE).build();
    SolrQuery solrQuery = VendorProductSolrHelper
        .getSolrQueryForVendorList(STORE_ID, summaryFilterDTO, new ArrayList<>(), pageable);
    Assertions.assertTrue(solrQuery.getQuery().contains(NOT_GOOD_EN));
  }

  @Test
   void getSolrQueryForVendorListTest_sortOrderDesc() {
    summaryFilterDTO = SummaryFilterDTO.builder().timeFilterType(TimeFilterType.ALL)
        .sortOrderByCreatedDate(ORDER_BY_DESC).vendorCode(VENDOR_CODE).build();
    SolrQuery solrQuery = VendorProductSolrHelper
        .getSolrQueryForVendorList(STORE_ID, summaryFilterDTO, new ArrayList<>(), pageable);
    Assertions.assertNotNull(solrQuery.getSorts());
  }

  @Test
   void getSolrQueryForVendorListTest_sortOrderAsc() {
    summaryFilterDTO = SummaryFilterDTO.builder().timeFilterType(TimeFilterType.ALL)
        .sortOrderByCreatedDate(ORDER_BY_ASC).vendorCode(VENDOR_CODE).build();
    SolrQuery solrQuery = VendorProductSolrHelper
        .getSolrQueryForVendorList(STORE_ID, summaryFilterDTO, new ArrayList<>(), pageable);
    Assertions.assertNotNull(solrQuery.getSorts());
  }

  @Test
   void getSolrQueryForProductListTest() {
    DistributionTaskMultipleFilterDTO distributionTaskMultipleFilterDTO = new DistributionTaskMultipleFilterDTO();
    distributionTaskMultipleFilterDTO.setStoreId(STORE_ID);
    SolrQuery solrQuery =
        VendorProductSolrHelper.getSolrQueryForProductList(distributionTaskMultipleFilterDTO, pageable);
    Assertions.assertTrue(solrQuery.getFilterQueries()[0].contains(STORE_ID));
    Assertions.assertTrue(solrQuery.getFilterQueries()[0].contains(MARK_FOR_DELETE));
  }

  @Test
   void getSolrQueryForProductListTest_withBusinessPartnerCode() {
    DistributionTaskMultipleFilterDTO distributionTaskMultipleFilterDTO = new DistributionTaskMultipleFilterDTO();
    distributionTaskMultipleFilterDTO.setStoreId(STORE_ID);
    distributionTaskMultipleFilterDTO.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    SolrQuery solrQuery =
        VendorProductSolrHelper.getSolrQueryForProductList(distributionTaskMultipleFilterDTO, pageable);
    Assertions.assertTrue(solrQuery.getFilterQueries()[0].contains(STORE_ID));
    Assertions.assertTrue(solrQuery.getQuery().contains(BUSINESS_PARTNER_CODE));
    Assertions.assertTrue(solrQuery.getFilterQueries()[0].contains(MARK_FOR_DELETE));
  }

  @Test
   void getSolrQueryForProductListTest_withBusinessPartnerCode_withAND() {
    DistributionTaskMultipleFilterDTO distributionTaskMultipleFilterDTO = new DistributionTaskMultipleFilterDTO();
    distributionTaskMultipleFilterDTO.setStoreId(STORE_ID);
    distributionTaskMultipleFilterDTO.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    distributionTaskMultipleFilterDTO.setCategoryCode(CATEGORY_CODE);
    SolrQuery solrQuery =
        VendorProductSolrHelper.getSolrQueryForProductList(distributionTaskMultipleFilterDTO, pageable);
    Assertions.assertTrue(solrQuery.getFilterQueries()[0].contains(STORE_ID));
    Assertions.assertTrue(solrQuery.getQuery().contains(BUSINESS_PARTNER_CODE));
    Assertions.assertTrue(solrQuery.getFilterQueries()[0].contains(MARK_FOR_DELETE));
  }

  @Test
   void getSolrQueryForProductListTest_withBusinessPartnerCodeExternal() {
    DistributionTaskMultipleFilterDTO distributionTaskMultipleFilterDTO = new DistributionTaskMultipleFilterDTO();
    distributionTaskMultipleFilterDTO.setStoreId(STORE_ID);
    distributionTaskMultipleFilterDTO.setBusinessPartnerCode("EXTERNAL");
    SolrQuery solrQuery =
        VendorProductSolrHelper.getSolrQueryForProductList(distributionTaskMultipleFilterDTO, pageable);
    Assertions.assertTrue(solrQuery.getFilterQueries()[0].contains(STORE_ID));
    Assertions.assertTrue(solrQuery.getFilterQueries()[0].contains(MARK_FOR_DELETE));
    Assertions.assertFalse(solrQuery.getQuery().contains("EXTERNAL"));
    Assertions.assertTrue(solrQuery.getQuery().contains("INTERNAL"));
  }

  @Test
   void getSolrQueryForFilterProductSummary_withBusinessPartnerCodeExternal() {
    ProductListRequest productListRequest = new ProductListRequest();
    productListRequest.setBusinessPartnerCode("EXTERNAL");
    SolrQuery solrQuery =
        VendorProductSolrHelper.getSolrQueryForFilterProductSummary(STORE_ID, productListRequest, new ArrayList<>(), pageable);
    Assertions.assertTrue(solrQuery.getFilterQueries()[0].contains(STORE_ID));
    Assertions.assertFalse(solrQuery.getQuery().contains("EXTERNAL"));
    Assertions.assertTrue(solrQuery.getQuery().contains("INTERNAL"));
  }

  @Test
   void toProductUpdateProductToSolrEventModelTest() {
    productReviewer.setApprovedDate(null);
    product.setSellerType(SellerType.NON_TRUSTED_SELLER);
    PDTProductUpdateProductToSolrEventModel response =
        VendorProductSolrHelper.toProductUpdateProductToSolrEventModel(product, productReviewer);
    Assertions.assertNull(response.getApprovedDate());
    Assertions.assertEquals(1, response.getReviewType());
    Assertions.assertEquals(WorkflowState.IN_REVIEW.name(), response.getState());
  }

  @Test
   void toProductUpdateProductToSolrEventModelStateNullTest() {
    product.setState(null);
    productReviewer.setApprovedDate(null);
    PDTProductUpdateProductToSolrEventModel response =
        VendorProductSolrHelper.toProductUpdateProductToSolrEventModel(product, productReviewer);
    Assertions.assertNull(response.getApprovedDate());
    Assertions.assertEquals(1, response.getReviewType());
    Assertions.assertEquals(StringUtils.EMPTY, response.getState());
  }

  @Test
   void toProductUpdateProductToSolrEventForForceReviewTest() {
    productReviewer.setApprovedDate(null);
    product.setPostLive(true);
    product.setForceReview(true);
    product.setSellerType(SellerType.NON_TRUSTED_SELLER);
    PDTProductUpdateProductToSolrEventModel response =
      VendorProductSolrHelper.toProductUpdateProductToSolrEventModel(product, productReviewer);
    Assertions.assertNull(response.getApprovedDate());
    Assertions.assertEquals(1, response.getReviewType());
    Assertions.assertEquals(WorkflowState.IN_REVIEW.name(), response.getState());
    Assertions.assertFalse(response.isPostLive());
  }

  @Test
   void toProductUpdateProductToSolrEventForForceReviewFalseTest() {
    productReviewer.setApprovedDate(null);
    product.setPostLive(true);
    product.setForceReview(false);
    PDTProductUpdateProductToSolrEventModel response =
      VendorProductSolrHelper.toProductUpdateProductToSolrEventModel(product, productReviewer);
    Assertions.assertNull(response.getApprovedDate());
    Assertions.assertEquals(1, response.getReviewType());
    Assertions.assertEquals(WorkflowState.IN_REVIEW.name(), response.getState());
    Assertions.assertTrue(response.isPostLive());
  }

  @Test
   void toProductUpdateProductToSolrEventForForceReview_forTrustedSellerTest() {
    productReviewer.setApprovedDate(null);
    product.setPostLive(true);
    product.setForceReview(true);
    product.setSellerType(SellerType.TRUSTED_SELLER);
    PDTProductUpdateProductToSolrEventModel response =
      VendorProductSolrHelper.toProductUpdateProductToSolrEventModel(product, productReviewer);
    Assertions.assertNull(response.getApprovedDate());
    Assertions.assertEquals(1, response.getReviewType());
    Assertions.assertEquals(WorkflowState.IN_REVIEW.name(), response.getState());
  }

  @Test
   void toProductUpdateProductToSolrEventForForceReviewFalse_forTrustedSellerTest() {
    productReviewer.setApprovedDate(null);
    product.setPostLive(true);
    product.setForceReview(false);
    product.setSellerType(SellerType.NON_TRUSTED_SELLER);
    PDTProductUpdateProductToSolrEventModel response =
      VendorProductSolrHelper.toProductUpdateProductToSolrEventModel(product, productReviewer);
    Assertions.assertNull(response.getApprovedDate());
    Assertions.assertEquals(1, response.getReviewType());
    Assertions.assertEquals(WorkflowState.IN_REVIEW.name(), response.getState());
    Assertions.assertTrue(response.isPostLive());
  }


  @Test
   void getSolrQueryForProductListTest_withProductName() {
    DistributionTaskMultipleFilterDTO distributionTaskMultipleFilterDTO = new DistributionTaskMultipleFilterDTO();
    distributionTaskMultipleFilterDTO.setProductName(PRODUCT_NAME);
    distributionTaskMultipleFilterDTO.setTimeFilterType(TimeFilterType.TODAY);
    SolrQuery solrQuery =
        VendorProductSolrHelper.getSolrQueryForProductList(distributionTaskMultipleFilterDTO, pageable);
    Assertions.assertTrue(solrQuery.getQuery().contains(PRODUCT_NAME));
  }

  @Test
   void getSolrQueryForProductListTest_withProductNameEmpty() {
    DistributionTaskMultipleFilterDTO distributionTaskMultipleFilterDTO = new DistributionTaskMultipleFilterDTO();
    distributionTaskMultipleFilterDTO.setProductName("");
    SolrQuery solrQuery =
        VendorProductSolrHelper.getSolrQueryForProductList(distributionTaskMultipleFilterDTO, pageable);
    Assertions.assertTrue(solrQuery.getQuery().contains(""));
  }

  @Test
   void getSolrQueryForProductListTest_withCategoryCode() {
    DistributionTaskMultipleFilterDTO distributionTaskMultipleFilterDTO = new DistributionTaskMultipleFilterDTO();
    distributionTaskMultipleFilterDTO.setCategoryCode(CATEGORY_CODE);
    SolrQuery solrQuery =
        VendorProductSolrHelper.getSolrQueryForProductList(distributionTaskMultipleFilterDTO, pageable);
    Assertions.assertTrue(solrQuery.getQuery().contains(CATEGORY_CODE));
  }

  @Test
   void getSolrQueryForProductListTest_withCategoryCode_withAND() {
    DistributionTaskMultipleFilterDTO distributionTaskMultipleFilterDTO = new DistributionTaskMultipleFilterDTO();
    distributionTaskMultipleFilterDTO.setCategoryCode(CATEGORY_CODE);
    distributionTaskMultipleFilterDTO.setProductName(PRODUCT_NAME);
    SolrQuery solrQuery =
        VendorProductSolrHelper.getSolrQueryForProductList(distributionTaskMultipleFilterDTO, pageable);
    Assertions.assertTrue(solrQuery.getQuery().contains(CATEGORY_CODE));
    Assertions.assertTrue(solrQuery.getQuery().contains(AND));
  }

  @Test
   void getSolrQueryForProductListTest_EmptyQuery() {
    DistributionTaskMultipleFilterDTO distributionTaskMultipleFilterDTO = new DistributionTaskMultipleFilterDTO();
    SolrQuery solrQuery =
        VendorProductSolrHelper.getSolrQueryForProductList(distributionTaskMultipleFilterDTO, pageable);
    Assertions.assertTrue(solrQuery.getQuery().contains(MARK_FOR_DELETE));
  }

  @Test
   void getSolrQueryForProductListTest_withStatusList() {
    DistributionTaskMultipleFilterDTO distributionTaskMultipleFilterDTO = new DistributionTaskMultipleFilterDTO();
    ArrayList<WorkflowState> states = new ArrayList<>();
    states.add(WorkflowState.PASSED);
    distributionTaskMultipleFilterDTO.setStatusList(states);
    SolrQuery solrQuery =
        VendorProductSolrHelper.getSolrQueryForProductList(distributionTaskMultipleFilterDTO, pageable);
    Assertions.assertTrue(solrQuery.getQuery().contains(STATE));
    Assertions.assertTrue(solrQuery.getQuery().contains(WorkflowState.PASSED.toString()));
  }

  @Test
   void getSolrQueryForProductListTest_withStatusList_withAND() {
    DistributionTaskMultipleFilterDTO distributionTaskMultipleFilterDTO = new DistributionTaskMultipleFilterDTO();
    ArrayList<WorkflowState> states = new ArrayList<>();
    states.add(WorkflowState.PASSED);
    distributionTaskMultipleFilterDTO.setStatusList(states);
    distributionTaskMultipleFilterDTO.setProductName(PRODUCT_NAME);
    SolrQuery solrQuery =
        VendorProductSolrHelper.getSolrQueryForProductList(distributionTaskMultipleFilterDTO, pageable);
    Assertions.assertTrue(solrQuery.getQuery().contains(STATE));
    Assertions.assertTrue(solrQuery.getQuery().contains(WorkflowState.PASSED.toString()));
  }

  @Test
   void getSolrQueryForProductListTest_timeFilterTypeToday() {
    DistributionTaskMultipleFilterDTO distributionTaskMultipleFilterDTO = new DistributionTaskMultipleFilterDTO();
    distributionTaskMultipleFilterDTO.setTimeFilterType(TimeFilterType.TODAY);
    SolrQuery solrQuery =
        VendorProductSolrHelper.getSolrQueryForProductList(distributionTaskMultipleFilterDTO, pageable);
    Assertions.assertTrue(solrQuery.getQuery().contains(
        SolrConstants.TODAY_FACET_INTERVAL.replace(SolrConstants.COMMA, StringUtils.SPACE + SolrConstants.TO)));
  }

  @Test
   void getSolrQueryForProductListTest_timeFilterTypeYesterday() {
    DistributionTaskMultipleFilterDTO distributionTaskMultipleFilterDTO = new DistributionTaskMultipleFilterDTO();
    distributionTaskMultipleFilterDTO.setTimeFilterType(TimeFilterType.YESTERDAY);
    SolrQuery solrQuery =
        VendorProductSolrHelper.getSolrQueryForProductList(distributionTaskMultipleFilterDTO, pageable);
    Assertions.assertTrue(solrQuery.getQuery().contains(
        SolrConstants.YESTERDAY_FACET_INTERVAL.replace(SolrConstants.COMMA, StringUtils.SPACE + SolrConstants.TO)));
  }

  @Test
   void getSolrQueryForProductListTest_timeFilterTypeTwoToThreeDays() {
    DistributionTaskMultipleFilterDTO distributionTaskMultipleFilterDTO = new DistributionTaskMultipleFilterDTO();
    distributionTaskMultipleFilterDTO.setTimeFilterType(TimeFilterType.TWO_TO_THREE_DAYS_OLD);
    SolrQuery solrQuery =
        VendorProductSolrHelper.getSolrQueryForProductList(distributionTaskMultipleFilterDTO, pageable);
    Assertions.assertTrue(solrQuery.getQuery().contains(SolrConstants.TWO_TO_THREE_DAYS_AGO_FACET_INTERVAL
        .replace(SolrConstants.COMMA, StringUtils.SPACE + SolrConstants.TO)));
  }

  @Test
   void getSolrQueryForProductListTest_timeFilterTypeThreeDaysAgo() {
    DistributionTaskMultipleFilterDTO distributionTaskMultipleFilterDTO = new DistributionTaskMultipleFilterDTO();
    distributionTaskMultipleFilterDTO.setTimeFilterType(TimeFilterType.THREE_DAYS_AGO);
    SolrQuery solrQuery =
        VendorProductSolrHelper.getSolrQueryForProductList(distributionTaskMultipleFilterDTO, pageable);
    Assertions.assertTrue(solrQuery.getQuery().contains(SolrConstants.THREE_DAYS_AGO_FACET_INTERVAL
        .replace(SolrConstants.COMMA, StringUtils.SPACE + SolrConstants.TO)));
  }

  @Test
   void getSolrQueryForProductListTest_timeFilterTypeToday_withAND() {
    DistributionTaskMultipleFilterDTO distributionTaskMultipleFilterDTO = new DistributionTaskMultipleFilterDTO();
    distributionTaskMultipleFilterDTO.setTimeFilterType(TimeFilterType.TODAY);
    distributionTaskMultipleFilterDTO.setProductName(PRODUCT_NAME);
    SolrQuery solrQuery =
        VendorProductSolrHelper.getSolrQueryForProductList(distributionTaskMultipleFilterDTO, pageable);
    Assertions.assertTrue(solrQuery.getQuery().contains(
        SolrConstants.TODAY_FACET_INTERVAL.replace(SolrConstants.COMMA, StringUtils.SPACE + SolrConstants.TO)));
  }

  @Test
   void getSolrQueryForProductListTest_withVendorCodes() {
    DistributionTaskMultipleFilterDTO distributionTaskMultipleFilterDTO = new DistributionTaskMultipleFilterDTO();
    ArrayList<String> vendorCodes = new ArrayList<>();
    vendorCodes.add("test1");
    vendorCodes.add("test2");
    distributionTaskMultipleFilterDTO.setVendorCodes(vendorCodes);
    SolrQuery solrQuery =
        VendorProductSolrHelper.getSolrQueryForProductList(distributionTaskMultipleFilterDTO, pageable);
    Assertions.assertTrue(solrQuery.getQuery().contains("test1"));
    Assertions.assertTrue(solrQuery.getQuery().contains("test2"));
    Assertions.assertTrue(solrQuery.getQuery().contains(VENDOR_CODE));
  }

  @Test
   void getSolrQueryForProductListTest_withVendorCodes_withAND() {
    DistributionTaskMultipleFilterDTO distributionTaskMultipleFilterDTO = new DistributionTaskMultipleFilterDTO();
    ArrayList<String> vendorCodes = new ArrayList<>();
    vendorCodes.add("test1");
    vendorCodes.add("test2");
    distributionTaskMultipleFilterDTO.setVendorCodes(vendorCodes);
    distributionTaskMultipleFilterDTO.setProductName(PRODUCT_NAME);
    SolrQuery solrQuery =
        VendorProductSolrHelper.getSolrQueryForProductList(distributionTaskMultipleFilterDTO, pageable);
    Assertions.assertTrue(solrQuery.getQuery().contains("test1"));
    Assertions.assertTrue(solrQuery.getQuery().contains("test2"));
    Assertions.assertTrue(solrQuery.getQuery().contains(VENDOR_CODE));
  }

  @Test
   void getSolrQueryForProductListTest_withRejectedListZero() {
    DistributionTaskMultipleFilterDTO distributionTaskMultipleFilterDTO = new DistributionTaskMultipleFilterDTO();
    ArrayList<Integer> rejectedList = new ArrayList<>();
    rejectedList.add(0);
    distributionTaskMultipleFilterDTO.setRejectedList(rejectedList);
    SolrQuery solrQuery =
        VendorProductSolrHelper.getSolrQueryForProductList(distributionTaskMultipleFilterDTO, pageable);
    Assertions.assertTrue(solrQuery.getQuery().contains("0"));
  }

  @Test
   void getSolrQueryForProductListTest_withRejectedListZero_withAND() {
    DistributionTaskMultipleFilterDTO distributionTaskMultipleFilterDTO = new DistributionTaskMultipleFilterDTO();
    ArrayList<Integer> rejectedList = new ArrayList<>();
    rejectedList.add(0);
    distributionTaskMultipleFilterDTO.setRejectedList(rejectedList);
    distributionTaskMultipleFilterDTO.setProductName(PRODUCT_NAME);
    SolrQuery solrQuery =
        VendorProductSolrHelper.getSolrQueryForProductList(distributionTaskMultipleFilterDTO, pageable);
    Assertions.assertTrue(solrQuery.getQuery().contains("0"));
  }

  @Test
   void getSolrQueryForProductListTest_withRejectedListOne() {
    DistributionTaskMultipleFilterDTO distributionTaskMultipleFilterDTO = new DistributionTaskMultipleFilterDTO();
    ArrayList<Integer> rejectedList = new ArrayList<>();
    rejectedList.add(1);
    distributionTaskMultipleFilterDTO.setRejectedList(rejectedList);
    SolrQuery solrQuery =
        VendorProductSolrHelper.getSolrQueryForProductList(distributionTaskMultipleFilterDTO, pageable);
    Assertions.assertTrue(solrQuery.getQuery().contains("1"));
  }

  @Test
   void getSolrQueryForProductListTest_withRejectedListGreaterThanOrEqualToTwo() {
    DistributionTaskMultipleFilterDTO distributionTaskMultipleFilterDTO = new DistributionTaskMultipleFilterDTO();
    ArrayList<Integer> rejectedList = new ArrayList<>();
    rejectedList.add(2);
    distributionTaskMultipleFilterDTO.setRejectedList(rejectedList);
    SolrQuery solrQuery =
        VendorProductSolrHelper.getSolrQueryForProductList(distributionTaskMultipleFilterDTO, pageable);
    Assertions.assertTrue(solrQuery.getQuery().contains("[2 TO *]"));
  }

  @Test
   void getSolrQueryForProductListTest_withRejectedListAll() {
    DistributionTaskMultipleFilterDTO distributionTaskMultipleFilterDTO = new DistributionTaskMultipleFilterDTO();
    ArrayList<Integer> rejectedList = new ArrayList<>();
    rejectedList.add(0);
    rejectedList.add(1);
    rejectedList.add(2);
    distributionTaskMultipleFilterDTO.setRejectedList(rejectedList);
    SolrQuery solrQuery =
        VendorProductSolrHelper.getSolrQueryForProductList(distributionTaskMultipleFilterDTO, pageable);
    Assertions.assertTrue(solrQuery.getQuery().contains("0"));
    Assertions.assertTrue(solrQuery.getQuery().contains("1"));
    Assertions.assertTrue(solrQuery.getQuery().contains("[2 TO *]"));
  }

  @Test
   void getSolrQueryForProductListTest_sortOrderAsc() {
    DistributionTaskMultipleFilterDTO distributionTaskMultipleFilterDTO = new DistributionTaskMultipleFilterDTO();
    distributionTaskMultipleFilterDTO.setSortOrderByCreatedDate(ORDER_BY_ASC);
    SolrQuery solrQuery =
        VendorProductSolrHelper.getSolrQueryForProductList(distributionTaskMultipleFilterDTO, pageable);
    Assertions.assertNotNull(solrQuery.getSorts());
  }

  @Test
   void getSolrQueryForProductListTest_sortOrderDesc() {
    DistributionTaskMultipleFilterDTO distributionTaskMultipleFilterDTO = new DistributionTaskMultipleFilterDTO();
    distributionTaskMultipleFilterDTO.setSortOrderByCreatedDate(ORDER_BY_DESC);
    SolrQuery solrQuery =
        VendorProductSolrHelper.getSolrQueryForProductList(distributionTaskMultipleFilterDTO, pageable);
    Assertions.assertNotNull(solrQuery.getSorts());
  }

  @Test
   void getSolrProductCollectionDTOTest_emptyDocument() {
    VendorProductSolr vendorProductSolr =
        VendorProductSolrHelper.getSolrProductCollectionDTO(new SolrDocument());
    Assertions.assertNotNull(vendorProductSolr);
  }

  @Test
   void getSolrProductCollectionDTOTest() {
    VendorProductSolr vendorProductSolr =
        VendorProductSolrHelper.getSolrProductCollectionDTO(vendorSolrInputDocument);
    Assertions.assertEquals(STORE_ID, vendorProductSolr.getStoreId());
    Assertions.assertEquals(PRODUCT_CODE, vendorProductSolr.getProductCode());
    Assertions.assertEquals(PRODUCT_NAME, vendorProductSolr.getProductName());
    Assertions.assertEquals(BUSINESS_PARTNER_CODE, vendorProductSolr.getBusinessPartnerCode());
    Assertions.assertEquals(CATEGORY_CODE, vendorProductSolr.getCategoryCodes().get(0));
    Assertions.assertEquals(CATEGORY_NAME, vendorProductSolr.getCategoryNames().get(0));
    Assertions.assertEquals(GOOD_EN, vendorProductSolr.getImageViolations());
    Assertions.assertEquals(BRAND_APPROVED_STATE, vendorProductSolr.getBrandApprovalStatus());
    Assertions.assertEquals(BRAND, vendorProductSolr.getBrand());
    Assertions.assertEquals(1, vendorProductSolr.getDistributionMappingStatus());
    Assertions.assertFalse(vendorProductSolr.isMarkForDelete());
    Assertions.assertFalse(vendorProductSolr.isPostLive());
    Assertions.assertEquals(WorkflowState.IN_REVIEW, vendorProductSolr.getState());
    Assertions.assertTrue(vendorProductSolr.isAppealedProduct());
    Assertions.assertEquals("PRODUCT_CREATION_TYPE", vendorProductSolr.getProductCreationType());
  }

  @Test
   void toProductTest() {
    vendorSolrInputDocument.setField(VendorProductSolrFieldNames.PREDICTED_BRAND, BRAND);
    vendorSolrInputDocument.setField(VendorProductSolrFieldNames.APPEALED_PRODUCT, false);
    vendorSolrInputDocument
      .setField(VendorProductSolrFieldNames.CATEGORY_CODES, List.of(CATEGORY_CODE,
        "CATEGORY_CODE_C1"));
    vendorSolrInputDocument
      .setField(VendorProductSolrFieldNames.CATEGORY_NAMES, List.of(CATEGORY_NAME,
        "CATEGORY_NAME_C1"));
    VendorProductSolr vendorProductSolr =
        VendorProductSolrHelper.getSolrProductCollectionDTO(vendorSolrInputDocument);
    Product result = VendorProductSolrHelper.toProduct(vendorProductSolr);
    Assertions.assertEquals(STORE_ID, result.getStoreId());
    Assertions.assertEquals(PRODUCT_CODE, result.getProductCode());
    Assertions.assertEquals(PRODUCT_NAME, result.getProductName());
    Assertions.assertEquals(BUSINESS_PARTNER_CODE, result.getBusinessPartnerCode());
    Assertions.assertEquals(CATEGORY_CODE, result.getCategoryCode());
    Assertions.assertEquals(CATEGORY_NAME, result.getCategoryName());
    Assertions.assertEquals(GOOD_EN, result.getImageViolations());
    Assertions.assertEquals(BRAND_APPROVED_STATE, result.getBrandApprovalStatus());
    Assertions.assertEquals(BRAND, result.getBrand());
    Assertions.assertFalse(result.isMarkForDelete());
    Assertions.assertFalse(result.isPostLive());
    Assertions.assertEquals(WorkflowState.IN_REVIEW, result.getState());
    Assertions.assertEquals(BRAND, result.getPredictedBrand());
    Assertions.assertFalse(result.isAppealedProduct());
    Assertions.assertEquals("CATEGORY_CODE_C1", result.getC1CategoryCode());
    Assertions.assertEquals("CATEGORY_NAME_C1", result.getC1CategoryName());
    Assertions.assertEquals("PRODUCT_CREATION_TYPE", vendorProductSolr.getProductCreationType());
  }

  @Test
   void toProductTest_fieldSetToNA() {
    vendorSolrInputDocument.setField(VendorProductSolrFieldNames.APPROVER_ASSIGNEE,
        SolrConstants.NOT_APPLICABLE);
    vendorSolrInputDocument
        .setField(VendorProductSolrFieldNames.VENDOR_CODE, SolrConstants.NOT_APPLICABLE);
    vendorSolrInputDocument.setField(VendorProductSolrFieldNames.IMAGE_VIOLATIONS,
        List.of(SolrConstants.NOT_APPLICABLE));
    VendorProductSolr vendorProductSolr =
        VendorProductSolrHelper.getSolrProductCollectionDTO(vendorSolrInputDocument);
    Product result = VendorProductSolrHelper.toProduct(vendorProductSolr);
    Assertions.assertEquals(STORE_ID, result.getStoreId());
    Assertions.assertEquals(PRODUCT_CODE, result.getProductCode());
    Assertions.assertEquals(PRODUCT_NAME, result.getProductName());
    Assertions.assertEquals(BUSINESS_PARTNER_CODE, result.getBusinessPartnerCode());
    Assertions.assertEquals(CATEGORY_CODE, result.getCategoryCode());
    Assertions.assertEquals(CATEGORY_NAME, result.getCategoryName());
    Assertions.assertNull(result.getImageViolations());
    Assertions.assertEquals(BRAND_APPROVED_STATE, result.getBrandApprovalStatus());
    Assertions.assertEquals(BRAND, result.getBrand());
    Assertions.assertFalse(result.isMarkForDelete());
    Assertions.assertFalse(result.isPostLive());
    Assertions.assertEquals(WorkflowState.IN_REVIEW, result.getState());
  }

  @Test
   void getSolrProductCollectionDTOSellerTypeTest() throws Exception {
    vendorSolrInputDocument
      .setField(VendorProductSolrFieldNames.SELLER_TYPE, SellerType.TRUSTED_SELLER.getValue());
    VendorProductSolr vendorProductSolr =
        VendorProductSolrHelper.getSolrProductCollectionDTO(vendorSolrInputDocument);
    Product result = VendorProductSolrHelper.toProduct(vendorProductSolr);
    Assertions.assertEquals(SellerType.TRUSTED_SELLER, result.getSellerType());
  }

  @Test
   void getInputForVendorMappingAtomicUpdateTest() {
    List<SolrInputDocument> solrInputDocumentList =
        VendorProductSolrHelper.getInputForVendorMappingAtomicUpdate(VENDOR_CODE,
            List.of(PRODUCT_CODE));
    Assertions.assertEquals(PRODUCT_CODE,
        solrInputDocumentList.get(0).get(VendorProductSolrFieldNames.PRODUCT_CODE).getValue());
    Assertions.assertEquals(Collections.singletonMap(SolrConstants.SET_CLAUSE, VENDOR_CODE),
        solrInputDocumentList.get(0).get(VendorProductSolrFieldNames.VENDOR_CODE).getFirstValue());
  }

  @Test
   void getInputForReviewerUpdateReviewerAssignedTest() {
    SolrInputDocument solrInputDocument =
        VendorProductSolrHelper.getInputForReviewerUpdate(PRODUCT_CODE, REVIEWER_ASSIGNED, new Date());
    Assertions.assertEquals(PRODUCT_CODE,
        solrInputDocument.get(VendorProductSolrFieldNames.PRODUCT_CODE).getValue());
    Assertions.assertEquals(Collections.singletonMap(SolrConstants.SET_CLAUSE, REVIEWER_ASSIGNED),
        solrInputDocument.get(VendorProductSolrFieldNames.APPROVER_ASSIGNEE)
            .getFirstValue());
    Assertions.assertEquals(Collections.singletonMap(SolrConstants.SET_CLAUSE, true),
        solrInputDocument.get(VendorProductSolrFieldNames.ASSIGNED)
            .getFirstValue());
  }

  @Test
   void getInputForReviewerUpdateReviewerUnassignedTest() {
    SolrInputDocument solrInputDocument =
        VendorProductSolrHelper.getInputForReviewerUpdate(PRODUCT_CODE, null, null);
    Assertions.assertEquals(PRODUCT_CODE,
        solrInputDocument.get(VendorProductSolrFieldNames.PRODUCT_CODE).getValue());
    Assertions.assertEquals(
        Collections.singletonMap(SolrConstants.SET_CLAUSE, SolrConstants.NOT_APPLICABLE),
        solrInputDocument.get(VendorProductSolrFieldNames.APPROVER_ASSIGNEE)
            .getFirstValue());
    Assertions.assertEquals(Collections.singletonMap(SolrConstants.SET_CLAUSE, null),
        solrInputDocument.get(VendorProductSolrFieldNames.ASSIGNED_DATE)
            .getFirstValue());
    Assertions.assertEquals(Collections.singletonMap(SolrConstants.SET_CLAUSE, false),
        solrInputDocument.get(VendorProductSolrFieldNames.ASSIGNED)
            .getFirstValue());
  }

  @Test
   void getInputForImageQcResponseTest() {
    productReviewer.setApproverAssignee(null);
    productReviewer.setApprovedDate(null);
    productReviewer.setAssignedDate(null);
    product.setPostLive(true);
    product.setForceReview(false);
    product.setTextViolations(new StringJoiner(",").add(IMAGE).add(IMAGE).toString());
    SolrInputDocument solrInputDocument = VendorProductSolrHelper
        .getInputForImageQcResponse(product, productReviewer);
    Assertions.assertEquals(PRODUCT_CODE,
        solrInputDocument.get(VendorProductSolrFieldNames.PRODUCT_CODE).getValue());
    Assertions.assertEquals(
        Collections.singletonMap(SolrConstants.SET_CLAUSE, Collections.singletonList(IMAGE)),
        solrInputDocument.get(VendorProductSolrFieldNames.IMAGE_VIOLATIONS).getFirstValue());
    Assertions.assertEquals(solrInputDocument.get(VendorProductSolrFieldNames.POST_LIVE).getValue(),
        Boolean.TRUE);
  }

  @Test
   void getInputForImageQcResponse_reviewObjectNullTest() {
    product.setForceReview(true);
    product.setPostLive(true);
    productReviewer.setApprovedDate(null);
    productReviewer.setAssignedDate(null);
    productReviewer.setApproverAssignee(null);
    product.setImageViolations(new StringJoiner(",").add(IMAGE).add(IMAGE).toString());
    product.setTextViolations(new StringJoiner(",").add(IMAGE).add(IMAGE).toString());
    SolrInputDocument solrInputDocument = VendorProductSolrHelper
      .getInputForImageQcResponse(product, productReviewer);
    Assertions.assertEquals(PRODUCT_CODE,
        solrInputDocument.get(VendorProductSolrFieldNames.PRODUCT_CODE).getValue());
    Assertions.assertEquals(
        Collections.singletonMap(SolrConstants.SET_CLAUSE, Collections.singletonList(IMAGE)),
        solrInputDocument.get(VendorProductSolrFieldNames.IMAGE_VIOLATIONS).getFirstValue());
    Assertions.assertNull(solrInputDocument.get(VendorProductSolrFieldNames.APPROVER_ASSIGNEE));
    Assertions.assertEquals(solrInputDocument.get(VendorProductSolrFieldNames.POST_LIVE).getValue(),
        Boolean.FALSE);
  }

  @Test
   void getInputForImageQcResponseNullTest() {
    product.setImageViolations(new StringJoiner(",").add(IMAGE).add(IMAGE).toString());
    SolrInputDocument solrInputDocument = VendorProductSolrHelper
        .getInputForImageQcResponse(product, null);
    Assertions.assertEquals(PRODUCT_CODE,
        solrInputDocument.get(VendorProductSolrFieldNames.PRODUCT_CODE).getValue());
    Assertions.assertEquals(
        Collections.singletonMap(SolrConstants.SET_CLAUSE, Collections.singletonList(IMAGE)),
        solrInputDocument.get(VendorProductSolrFieldNames.IMAGE_VIOLATIONS).getFirstValue());
  }

  @Test
   void getInputForImageQcResponseTest_emptyImageViolations() {
    SolrInputDocument solrInputDocument = VendorProductSolrHelper
        .getInputForImageQcResponse(product, productReviewer);
    Assertions.assertEquals(PRODUCT_CODE,
        solrInputDocument.get(VendorProductSolrFieldNames.PRODUCT_CODE).getValue());
    Assertions.assertEquals(Collections
            .singletonMap(SolrConstants.SET_CLAUSE, List.of(SolrConstants.NOT_APPLICABLE)),
        solrInputDocument.get(VendorProductSolrFieldNames.IMAGE_VIOLATIONS).getFirstValue());
  }

  @Test
   void getInputForImageQcResponseTest_emptyProduct() {
    SolrInputDocument solrInputDocument = VendorProductSolrHelper
        .getInputForImageQcResponse(product, productReviewer);
    Assertions.assertEquals(PRODUCT_CODE,
        solrInputDocument.get(VendorProductSolrFieldNames.PRODUCT_CODE).getValue());
  }

  @Test
   void getInputForImageQcResponseTestEmptyProduct() {
    Product product2 = new Product();
    product2.setProductCode(PRODUCT_CODE);
    SolrInputDocument solrInputDocument = VendorProductSolrHelper
        .getInputForImageQcResponse(product, productReviewer);
    Assertions.assertEquals(PRODUCT_CODE,
        solrInputDocument.get(VendorProductSolrFieldNames.PRODUCT_CODE).getValue());
  }

  @Test
   void getQueryForReviewConfigCountsTest() {
    SolrQuery solrQuery =
        VendorProductSolrHelper.getQueryForReviewConfigCounts(STORE_ID, VENDOR_CODE);
    Assertions.assertNotNull(solrQuery);
    Assertions.assertEquals(VENDOR_CODE + SolrConstants.COLON + VENDOR_CODE,
        solrQuery.getFilterQueries()[1]);
    Assertions.assertEquals(VendorProductSolrFieldNames.POST_LIVE, solrQuery.getFacetFields()[0]);
    Assertions.assertTrue(solrQuery.getQuery().contains(WorkflowState.PASSED.toString()) && solrQuery.getQuery()
        .contains(WorkflowState.PASSED.toString()) && solrQuery.getQuery()
        .contains(SolrConstants.NOT));
  }

  @Test
   void getQueryForReviewCountsForConfigTest() {
    SolrQuery solrQuery =
        VendorProductSolrHelper.getQueryForReviewCountsForConfig(STORE_ID, VENDOR_CODE, Boolean.FALSE);
    Assertions.assertNotNull(solrQuery);
    Assertions.assertEquals(VENDOR_CODE + SolrConstants.COLON + VENDOR_CODE,
        solrQuery.getFilterQueries()[1]);
    Assertions.assertEquals(POST_LIVE + SolrConstants.COLON + Boolean.FALSE,
        solrQuery.getFilterQueries()[2]);
    Assertions.assertEquals(VendorProductSolrFieldNames.PRODUCT_REVIEW_TYPE,
        solrQuery.getFacetFields()[0]);
    Assertions.assertTrue(solrQuery.getQuery().contains(
      new StringBuilder().append(SolrConstants.NOT).append(VendorProductSolrFieldNames.STATE)
        .append(SolrConstants.COLON).append(SolrConstants.OPEN_BRACKET).append(WorkflowState.PASSED)
        .append(SolrConstants.CLOSE_BRACKET).toString()));
  }

  @Test
   void getSolrQueryForFilterCountsTest() {
    SolrQuery solrQuery =
        VendorProductSolrHelper.getSolrQueryForFilterCounts(STORE_ID, VENDOR_CODE, false, false,
          false);
    Assertions.assertNotNull(solrQuery);
    Assertions.assertEquals(solrQuery.getFilterQueries().length, 5);
    Assertions.assertEquals(solrQuery.getFacetFields().length, 3);
    Assertions.assertTrue(Arrays.asList(solrQuery.getFacetFields())
      .contains(VendorProductSolrFieldNames.RESTRICTED_KEYWORDS_PRESENT));
  }

  @Test
   void getSolrQueryForFilterCounts_editedTest() {
    SolrQuery solrQuery =
      VendorProductSolrHelper.getSolrQueryForFilterCounts(STORE_ID, VENDOR_CODE, true, true,
        false);
    Assertions.assertNotNull(solrQuery);
    Assertions.assertEquals(solrQuery.getFilterQueries().length, 5);
    Assertions.assertEquals(solrQuery.getFacetFields().length, 3);
  }

  @Test
   void getSolrQueryForFilterCounts_revisedTest() {
    SolrQuery solrQuery =
      VendorProductSolrHelper.getSolrQueryForFilterCounts(STORE_ID, VENDOR_CODE, false, true,
        true);
    Assertions.assertNotNull(solrQuery);
    Assertions.assertEquals(solrQuery.getFilterQueries().length, 5);
    Assertions.assertEquals(solrQuery.getFacetFields().length, 3);
  }

  @Test
   void getSolrQueryForFilterCounts_newlyAddedTest() {
    SolrQuery solrQuery =
      VendorProductSolrHelper.getSolrQueryForFilterCounts(STORE_ID, VENDOR_CODE, false, true,
        false);
    Assertions.assertNotNull(solrQuery);
    System.out.println(solrQuery);
    Assertions.assertEquals(solrQuery.getFilterQueries().length, 5);
    Assertions.assertEquals(solrQuery.getFacetFields().length, 2);
    Assertions.assertTrue(
        Arrays.stream(solrQuery.getFilterQueries()).anyMatch((t)->t.contains("productReviewType:0")));
  }

  @Test
   void getSolrQueryForFilterCounts_noFilterForProductReview() {
    SolrQuery solrQuery =
      VendorProductSolrHelper.getSolrQueryForFilterCounts(STORE_ID, VENDOR_CODE, null, true,
        null);
    Assertions.assertNotNull(solrQuery);
    System.out.println(solrQuery);
    Assertions.assertEquals(solrQuery.getFilterQueries().length, 5);
    Assertions.assertEquals(solrQuery.getFacetFields().length, 3);
    Assertions.assertTrue(Arrays.stream(solrQuery.getFilterQueries()).anyMatch((t)->t.contains(
      "productReviewType")));
  }

  @Test
   void getSolrQueryForFilterCountsNotRevisedTest() {
    SolrQuery solrQuery =
      VendorProductSolrHelper.getSolrQueryForFilterCounts(STORE_ID, VENDOR_CODE, true, true,
        true);
    Assertions.assertNotNull(solrQuery);
    System.out.println(solrQuery);
    Assertions.assertEquals(solrQuery.getFilterQueries().length, 4);
    Assertions.assertEquals(solrQuery.getFacetFields().length, 3);
    Assertions.assertTrue(Arrays.stream(solrQuery.getFilterQueries()).noneMatch((t)->t.contains(
      "productReviewType")));
  }

  @Test
   void getSolrQueryForFilterCountsNullPostLiveTest() {
    SolrQuery solrQuery =
      VendorProductSolrHelper.getSolrQueryForFilterCounts(STORE_ID, VENDOR_CODE, true, null,
        true);
    Assertions.assertNotNull(solrQuery);
    System.out.println(solrQuery);
    Assertions.assertEquals(solrQuery.getFilterQueries().length, 4);
    Assertions.assertEquals(solrQuery.getFacetFields().length, 4);
    Assertions.assertTrue(Arrays.stream(solrQuery.getFilterQueries()).noneMatch((t)->t.contains(
      "productReviewType")));
  }

  @Test
   void getSolrQueryForFilterCountsEditedNullRevisedNotNullTest() {
    SolrQuery solrQuery =
      VendorProductSolrHelper.getSolrQueryForFilterCounts(STORE_ID, VENDOR_CODE, null, null,
        true);
    Assertions.assertNotNull(solrQuery);
    System.out.println(solrQuery);
    Assertions.assertEquals(solrQuery.getFilterQueries().length, 4);
    Assertions.assertEquals(solrQuery.getFacetFields().length, 4);
    Assertions.assertTrue(Arrays.stream(solrQuery.getFilterQueries()).noneMatch((t)->t.contains(
      "productReviewType")));
  }

  @Test
   void getSolrQueryForFilterCounts_notNewlyAddedTest() {
    SolrQuery solrQuery =
      VendorProductSolrHelper.getSolrQueryForFilterCounts(STORE_ID, VENDOR_CODE, false, true,
        null);
    Assertions.assertNotNull(solrQuery);
    System.out.println(solrQuery);
    Assertions.assertEquals(solrQuery.getFilterQueries().length, 4);
    Assertions.assertEquals(solrQuery.getFacetFields().length, 2);
    Assertions.assertTrue(Arrays.stream(solrQuery.getFilterQueries()).noneMatch((t)->t.contains(
      "productReviewType")));
  }

  @Test
   void getSolrQueryForFinalQcCountsTest() {
    SolrQuery solrQuery =
        VendorProductSolrHelper.getSolrQueryForFinalQcCounts(STORE_ID);
    Assertions.assertNotNull(solrQuery);
    Assertions.assertEquals(2, solrQuery.getFilterQueries().length);
  }

  @Test
   void getSolrQueryForDistributionListCountsTest() {
    SolrQuery solrQuery =
        VendorProductSolrHelper.getSolrQueryForDistributionListCounts(STORE_ID);
    Assertions.assertNotNull(solrQuery);
    Assertions.assertEquals(1, solrQuery.getFilterQueries().length);
  }

  @Test
   void getSolrQueryForBusinessPartnerListTest() {
    primaryFilterDTO.setAssignment(true);
    primaryFilterDTO.setBrandPending(true);
    primaryFilterDTO.setTimeFilterType(TimeFilterType.ALL);
    primaryFilterDTO.setContentPending(true);
    primaryFilterDTO.setImagePending(true);
    primaryFilterDTO.setEdited(true);
    primaryFilterDTO.setPostLive(true);
    primaryFilterDTO.setVendorCode(VENDOR_CODE);
    primaryFilterDTO.setKeyword(PRODUCT_NAME);
    primaryFilterDTO.setRevised(true);
    primaryFilterDTO.setRestrictedKeyword(true);
    SolrQuery solrQuery = VendorProductSolrHelper.getSolrQueryForBusinessPartnerList(STORE_ID, primaryFilterDTO, pendingStates);
    Assertions.assertNotNull(solrQuery);
    Assertions.assertEquals(solrQuery.getFilterQueries().length, 10);
    Assertions.assertEquals(solrQuery.getSortField(),
        VendorProductSolrFieldNames.COPY_BUSINESS_PARTNER_NAME + StringUtils.SPACE + SolrQuery.ORDER.asc);
  }

  @Test
   void getSolrQueryForBusinessPartnerListContentPendingNullTest() {
    primaryFilterDTO.setAssignment(true);
    primaryFilterDTO.setBrandPending(true);
    primaryFilterDTO.setTimeFilterType(TimeFilterType.ALL);
    primaryFilterDTO.setContentPending(null);
    primaryFilterDTO.setImagePending(true);
    primaryFilterDTO.setEdited(true);
    primaryFilterDTO.setPostLive(true);
    primaryFilterDTO.setVendorCode(VENDOR_CODE);
    primaryFilterDTO.setKeyword(PRODUCT_NAME);
    primaryFilterDTO.setRevised(true);
    primaryFilterDTO.setRestrictedKeyword(true);
    SolrQuery solrQuery = VendorProductSolrHelper.getSolrQueryForBusinessPartnerList(STORE_ID, primaryFilterDTO, pendingStates);
    Assertions.assertNotNull(solrQuery);
    Assertions.assertEquals(solrQuery.getFilterQueries().length, 10);
    Assertions.assertEquals(solrQuery.getSortField(),
        VendorProductSolrFieldNames.COPY_BUSINESS_PARTNER_NAME + StringUtils.SPACE + SolrQuery.ORDER.asc);
  }

  @Test
   void getSolrQueryForBusinessPartnerListContentPendingAndImagePendingNullTest() {
    primaryFilterDTO.setAssignment(true);
    primaryFilterDTO.setBrandPending(true);
    primaryFilterDTO.setTimeFilterType(TimeFilterType.ALL);
    primaryFilterDTO.setContentPending(null);
    primaryFilterDTO.setImagePending(null);
    primaryFilterDTO.setEdited(true);
    primaryFilterDTO.setPostLive(true);
    primaryFilterDTO.setVendorCode(VENDOR_CODE);
    primaryFilterDTO.setKeyword(PRODUCT_NAME);
    primaryFilterDTO.setRevised(true);
    primaryFilterDTO.setRestrictedKeyword(true);
    SolrQuery solrQuery = VendorProductSolrHelper.getSolrQueryForBusinessPartnerList(STORE_ID, primaryFilterDTO, pendingStates);
    Assertions.assertNotNull(solrQuery);
    Assertions.assertEquals(solrQuery.getFilterQueries().length, 9);
    Assertions.assertEquals(solrQuery.getSortField(),
        VendorProductSolrFieldNames.COPY_BUSINESS_PARTNER_NAME + StringUtils.SPACE + SolrQuery.ORDER.asc);
  }

  @Test
   void getSolrQueryForBusinessPartnerListEmptyKeywordTest() {
    primaryFilterDTO.setAssignment(true);
    primaryFilterDTO.setBrandPending(true);
    primaryFilterDTO.setTimeFilterType(TimeFilterType.ALL);
    primaryFilterDTO.setContentPending(true);
    primaryFilterDTO.setImagePending(true);
    primaryFilterDTO.setEdited(true);
    primaryFilterDTO.setPostLive(true);
    primaryFilterDTO.setVendorCode(VENDOR_CODE);
    primaryFilterDTO.setKeyword(StringUtils.EMPTY);
    primaryFilterDTO.setRevised(true);
    primaryFilterDTO.setRestrictedKeyword(true);
    SolrQuery solrQuery = VendorProductSolrHelper.getSolrQueryForBusinessPartnerList(STORE_ID, primaryFilterDTO, pendingStates);
    Assertions.assertNotNull(solrQuery);
    Assertions.assertEquals(solrQuery.getFilterQueries().length, 10);
    Assertions.assertEquals(solrQuery.getSortField(),
        VendorProductSolrFieldNames.COPY_BUSINESS_PARTNER_NAME + StringUtils.SPACE + SolrQuery.ORDER.asc);
  }

  @Test
   void getBusinessPartnerListAllFalse() {
    primaryFilterDTO.setAssignment(false);
    primaryFilterDTO.setBrandPending(false);
    primaryFilterDTO.setTimeFilterType(TimeFilterType.FIVE_DAYS_AGO);
    primaryFilterDTO.setContentPending(false);
    primaryFilterDTO.setImagePending(false);
    primaryFilterDTO.setEdited(false);
    primaryFilterDTO.setPostLive(null);
    primaryFilterDTO.setVendorCode(VENDOR_CODE);
    primaryFilterDTO.setKeyword(SolrConstants.EXTERNAL);
    primaryFilterDTO.setRevised(false);
    primaryFilterDTO.setRestrictedKeyword(false);
    SolrQuery solrQuery =
        VendorProductSolrHelper.getSolrQueryForBusinessPartnerList(STORE_ID, primaryFilterDTO, pendingStates);
    Assertions.assertNotNull(solrQuery);
    Assertions.assertEquals(solrQuery.getFilterQueries().length, 9);
    Assertions.assertEquals(solrQuery.getSortField(),
        VendorProductSolrFieldNames.COPY_BUSINESS_PARTNER_NAME + StringUtils.SPACE + SolrQuery.ORDER.asc);
  }

  @Test
   void getBusinessPartnerListAllFalseTest() {
    primaryFilterDTO.setAssignment(false);
    primaryFilterDTO.setBrandPending(null);
    primaryFilterDTO.setTimeFilterType(TimeFilterType.FIVE_DAYS_AGO);
    primaryFilterDTO.setContentPending(false);
    primaryFilterDTO.setImagePending(false);
    primaryFilterDTO.setEdited(false);
    primaryFilterDTO.setPostLive(null);
    primaryFilterDTO.setVendorCode(VENDOR_CODE);
    primaryFilterDTO.setKeyword(SolrConstants.EXTERNAL);
    primaryFilterDTO.setRevised(false);
    primaryFilterDTO.setRestrictedKeyword(null);
    SolrQuery solrQuery =
        VendorProductSolrHelper.getSolrQueryForBusinessPartnerList(STORE_ID, primaryFilterDTO, pendingStates);
    Assertions.assertNotNull(solrQuery);
    Assertions.assertEquals(solrQuery.getFilterQueries().length, 8);
    Assertions.assertEquals(solrQuery.getSortField(),
        VendorProductSolrFieldNames.COPY_BUSINESS_PARTNER_NAME + StringUtils.SPACE + SolrQuery.ORDER.asc);
  }

  @Test
   void getBusinessPartnerListAllFalseTestAssignmentNull() {
    primaryFilterDTO.setAssignment(null);
    primaryFilterDTO.setBrandPending(null);
    primaryFilterDTO.setTimeFilterType(TimeFilterType.FIVE_DAYS_AGO);
    primaryFilterDTO.setContentPending(false);
    primaryFilterDTO.setImagePending(false);
    primaryFilterDTO.setEdited(false);
    primaryFilterDTO.setPostLive(null);
    primaryFilterDTO.setVendorCode(VENDOR_CODE);
    primaryFilterDTO.setKeyword(SolrConstants.EXTERNAL);
    primaryFilterDTO.setRevised(false);
    primaryFilterDTO.setRestrictedKeyword(null);
    SolrQuery solrQuery =
        VendorProductSolrHelper.getSolrQueryForBusinessPartnerList(STORE_ID, primaryFilterDTO, pendingStates);
    Assertions.assertNotNull(solrQuery);
    Assertions.assertEquals(solrQuery.getFilterQueries().length, 7);
    Assertions.assertEquals(solrQuery.getSortField(),
        VendorProductSolrFieldNames.COPY_BUSINESS_PARTNER_NAME + StringUtils.SPACE + SolrQuery.ORDER.asc);
  }

  @Test
   void getSolrInputDocumentOnApprovalOrSaveTest() {
    PDTProductUpdateProductToSolrEventModel product = new PDTProductUpdateProductToSolrEventModel();
    product.setEdited(true);
    product.setBrandApprovalStatus(BrandApprovalStatus.APPROVED.name());
    SolrInputDocument solrInputFields =
        VendorProductSolrHelper.getSolrInputDocumentOnApprovalOrSave(product, new ArrayList<>());
    Assertions.assertNotNull(solrInputFields);
    Assertions.assertNull(solrInputFields.getField(VendorProductSolrFieldNames.CATEGORY_CODES));
    HashMap hashMap = new HashMap();
    hashMap.put(SolrConstants.SET, (BrandApprovalStatus.valueOf(product.getBrandApprovalStatus())).getValue());
    Assertions.assertEquals(hashMap,
        solrInputFields.getField(VendorProductSolrFieldNames.BRAND_APPROVAL_STATUS).getValue());
  }

  @Test
   void getSolrInputDocumentOnApprovalOrSaveSellerBadgeTest() {
    PDTProductUpdateProductToSolrEventModel product = new PDTProductUpdateProductToSolrEventModel();
    product.setEdited(true);
    product.setBrandApprovalStatus(BrandApprovalStatus.APPROVED.name());
    product.setSellerBadge(SellerBadge.BRONZE_MERCHANT.name());
    SolrInputDocument solrInputFields =
      VendorProductSolrHelper.getSolrInputDocumentOnApprovalOrSave(product, new ArrayList<>());
    Assertions.assertNotNull(solrInputFields);
    Assertions.assertNull(solrInputFields.getField(VendorProductSolrFieldNames.CATEGORY_CODES));
    HashMap hashMap = new HashMap();
    hashMap.put(SolrConstants.SET, (BrandApprovalStatus.valueOf(product.getBrandApprovalStatus())).getValue());
    Assertions.assertEquals(hashMap,
        solrInputFields.getField(VendorProductSolrFieldNames.BRAND_APPROVAL_STATUS).getValue());
    Assertions.assertEquals(List.of(SellerBadge.BRONZE_MERCHANT.getValue()),
        solrInputFields.getField(VendorProductSolrFieldNames.SELLER_BADGE).getValue());
  }

  @Test
   void getSolrInputDocumentOnApprovalOrSaveWithCategoryChangeTest() {
    PDTProductUpdateProductToSolrEventModel product =
      new PDTProductUpdateProductToSolrEventModel();
    product.setBrandApprovalStatus(BrandApprovalStatus.APPROVED.name());
    product.setEdited(true);
    SolrInputDocument solrInputFields =
        VendorProductSolrHelper.getSolrInputDocumentOnApprovalOrSave(product,
            Collections.singletonList(categoryResponse));
    Assertions.assertNotNull(solrInputFields);
    HashMap hashMap = new HashMap();
    hashMap.put(SolrConstants.SET, (BrandApprovalStatus.valueOf(product.getBrandApprovalStatus())).getValue());
    Assertions.assertEquals(hashMap,
        solrInputFields.getField(VendorProductSolrFieldNames.BRAND_APPROVAL_STATUS).getValue());
  }

  @Test
  void getSolrInputDocumentOnApprovalOrSaveWithCategoryChangeTest_brandAprpovalStatusisNull() {
    PDTProductUpdateProductToSolrEventModel product =
        new PDTProductUpdateProductToSolrEventModel();
    product.setEdited(true);
    SolrInputDocument solrInputFields =
        VendorProductSolrHelper.getSolrInputDocumentOnApprovalOrSave(product,
            Collections.singletonList(categoryResponse));
    Assertions.assertNotNull(solrInputFields);
    HashMap hashMap = new HashMap();
    hashMap.put(SolrConstants.SET, (BrandApprovalStatus.APPROVED).getValue());
    Assertions.assertEquals(hashMap,
        solrInputFields.getField(VendorProductSolrFieldNames.BRAND_APPROVAL_STATUS).getValue());
  }

  @Test
   void getSolrInputDocumentOnContentApprovalOrSave() {
    product.setBrandApprovalStatus(BRAND_APPROVED_STATE);
    SolrInputDocument solrInputFields =
        VendorProductSolrHelper.getSolrInputDocumentOnContentApprovalOrSave(product, productReviewer, new ArrayList<>());
    Assertions.assertNotNull(solrInputFields);
    Assertions.assertNull(solrInputFields.getField(VendorProductSolrFieldNames.CATEGORY_CODES));
    HashMap hashMap = new HashMap();
    hashMap.put(SolrConstants.SET, product.getBrandApprovalStatus());
    vendorSolrInputDocument
        .setField(VendorProductSolrFieldNames.BRAND_APPROVAL_STATUS, BrandApprovalStatus.APPROVED.getValue());
  }

  @Test
   void getSolrInputDocumentOnContentApprovalOrSaveWithCategoryChange() {
    SolrInputDocument solrInputFields =
        VendorProductSolrHelper.getSolrInputDocumentOnContentApprovalOrSave(product, productReviewer,
            Collections.singletonList(categoryResponse));
    Assertions.assertNotNull(solrInputFields);
    Assertions.assertNotNull(solrInputFields.getField(VendorProductSolrFieldNames.CATEGORY_CODES));
  }

  @Test
   void getSolrInputDocumentOnImageApprovalOrSave() {
    product.setImageViolations(IMAGE_VIOLATION);
    SolrInputDocument solrInputFields = VendorProductSolrHelper.getSolrInputDocumentOnImageApproval(product, productReviewer);
    Assertions.assertNotNull(solrInputFields);
  }

  @Test
   void toVendorProductSolrTest() {
    product.setCurrentVendor(null);
    product.setReviewType(ReviewType.CONTENT);
    productReviewer.setApproverAssignee(null);
    product.setImageViolations(PRODUCT_CODE);
    product.setTextViolations(PRODUCT_CODE);
    product.setPredictedBrand(BRAND);
    VendorProductSolr vendorProductSolr = VendorProductSolrHelper.toVendorProductSolr(product, productReviewer,
      imageQcProcessedAndBrandResponse);
    Assertions.assertNotNull(vendorProductSolr);
    Assertions.assertEquals(PRODUCT_NAME, vendorProductSolr.getProductName());
    Assertions.assertEquals(PRODUCT_CODE, vendorProductSolr.getProductCode());
    Assertions.assertEquals(SolrConstants.NOT_APPLICABLE,
        vendorProductSolr.getProductApproverAssignee());
    Assertions.assertEquals(SolrConstants.NOT_APPLICABLE, vendorProductSolr.getVendorCode());
    Assertions.assertEquals(ReviewType.CONTENT.name(), vendorProductSolr.getReviewType());
    Assertions.assertEquals(PRODUCT_CODE, vendorProductSolr.getImageViolations());
    Assertions.assertEquals(BRAND, vendorProductSolr.getPredictedBrand());
  }

  @Test
   void toVendorProductSolrTestReviewTypeNull() {
    product.setCurrentVendor(null);
    product.setReviewType(null);
    productReviewer.setApproverAssignee(null);
    VendorProductSolr vendorProductSolr = VendorProductSolrHelper.toVendorProductSolr(product, productReviewer,
      imageQcProcessedAndBrandResponse);
    Assertions.assertNotNull(vendorProductSolr);
    Assertions.assertEquals(PRODUCT_NAME, vendorProductSolr.getProductName());
    Assertions.assertEquals(PRODUCT_CODE, vendorProductSolr.getProductCode());
    Assertions.assertEquals(SolrConstants.NOT_APPLICABLE,
        vendorProductSolr.getProductApproverAssignee());
    Assertions.assertEquals(SolrConstants.NOT_APPLICABLE, vendorProductSolr.getVendorCode());
    Assertions.assertNull(vendorProductSolr.getReviewType());
  }

  @Test
   void toVendorProductSolrTest_withReviewers() {
    product.setCurrentVendor(null);
    VendorProductSolr vendorProductSolr = VendorProductSolrHelper.toVendorProductSolr(product, productReviewer,
      imageQcProcessedAndBrandResponse);
    Assertions.assertNotNull(vendorProductSolr);
    Assertions.assertEquals(PRODUCT_NAME, vendorProductSolr.getProductName());
    Assertions.assertEquals(PRODUCT_CODE, vendorProductSolr.getProductCode());
    Assertions.assertEquals(SolrConstants.NOT_APPLICABLE, vendorProductSolr.getVendorCode());
    Assertions.assertEquals(ASSIGNEE_EMAIL, vendorProductSolr.getProductApproverAssignee());
  }

  @Test
   void toVendorProductSolrAssigneeTest() {
    product.setCurrentVendor(null);
    Date date = new Date();
    product.setPostLive(false);
    imageQcProcessedAndBrandResponse.setImageQcProcessedResponse(
      ImageQcProcessedResponse.builder().forceReview(Boolean.FALSE).build());
    productReviewer.setApprovedDate(date);
    productReviewer.setAssignedDate(date);
    VendorProductSolr vendorProductSolr = VendorProductSolrHelper.toVendorProductSolr(product, productReviewer,
      imageQcProcessedAndBrandResponse);
    Assertions.assertNotNull(vendorProductSolr);
    Assertions.assertEquals(PRODUCT_NAME, vendorProductSolr.getProductName());
    Assertions.assertEquals(PRODUCT_CODE, vendorProductSolr.getProductCode());
    Assertions.assertEquals(SolrConstants.NOT_APPLICABLE, vendorProductSolr.getVendorCode());
    Assertions.assertEquals(ASSIGNEE_EMAIL, vendorProductSolr.getProductApproverAssignee());
    Assertions.assertEquals(date, vendorProductSolr.getProductApprovedDate());
    Assertions.assertEquals(date, vendorProductSolr.getProductAssignedDate());
  }

  @Test
   void toVendorProductSolrTest_currentVendorNotEmpty() {
    productReviewer.setApproverAssignee(null);
    product.setCurrentVendor(new Vendor());
    product.getCurrentVendor().setVendorCode(VENDOR_CODE);
    VendorProductSolr vendorProductSolr = VendorProductSolrHelper.toVendorProductSolr(product, productReviewer,
      imageQcProcessedAndBrandResponse);
    Assertions.assertNotNull(vendorProductSolr);
    Assertions.assertEquals(PRODUCT_NAME, vendorProductSolr.getProductName());
    Assertions.assertEquals(PRODUCT_CODE, vendorProductSolr.getProductCode());
    Assertions.assertEquals(SolrConstants.NOT_APPLICABLE,
        vendorProductSolr.getProductApproverAssignee());
    Assertions.assertEquals(VENDOR_CODE, vendorProductSolr.getVendorCode());
  }

  @Test
   void toVendorProductSolr_withForceReview() {
    productReviewer.setApproverAssignee(null);
    product.setPostLive(Boolean.TRUE);
    imageQcProcessedAndBrandResponse.setImageQcProcessedResponse(
      ImageQcProcessedResponse.builder().forceReview(Boolean.TRUE).build());
    product.setCurrentVendor(new Vendor());
    product.getCurrentVendor().setVendorCode(VENDOR_CODE);
    VendorProductSolr vendorProductSolr = VendorProductSolrHelper.toVendorProductSolr(product, productReviewer,
      imageQcProcessedAndBrandResponse);
    Assertions.assertNotNull(vendorProductSolr);
    Assertions.assertEquals(PRODUCT_NAME, vendorProductSolr.getProductName());
    Assertions.assertEquals(PRODUCT_CODE, vendorProductSolr.getProductCode());
    Assertions.assertEquals(SolrConstants.NOT_APPLICABLE,
        vendorProductSolr.getProductApproverAssignee());
    Assertions.assertEquals(VENDOR_CODE, vendorProductSolr.getVendorCode());
    Assertions.assertFalse(vendorProductSolr.isPostLive());
  }

  @Test
   void toVendorProductSolr_withForceReviewForTrustedSellers() {
    productReviewer.setApproverAssignee(null);
    product.setPostLive(Boolean.TRUE);
    product.setSellerType(SellerType.TRUSTED_SELLER);
    imageQcProcessedAndBrandResponse.setImageQcProcessedResponse(
      ImageQcProcessedResponse.builder().forceReview(Boolean.TRUE).build());
    product.setCurrentVendor(new Vendor());
    product.getCurrentVendor().setVendorCode(VENDOR_CODE);
    VendorProductSolr vendorProductSolr = VendorProductSolrHelper.toVendorProductSolr(product, productReviewer,
      imageQcProcessedAndBrandResponse);
    Assertions.assertNotNull(vendorProductSolr);
    Assertions.assertEquals(PRODUCT_NAME, vendorProductSolr.getProductName());
    Assertions.assertEquals(PRODUCT_CODE, vendorProductSolr.getProductCode());
    Assertions.assertEquals(SolrConstants.NOT_APPLICABLE,
        vendorProductSolr.getProductApproverAssignee());
    Assertions.assertEquals(VENDOR_CODE, vendorProductSolr.getVendorCode());
    Assertions.assertTrue(vendorProductSolr.isPostLive());
    Assertions.assertEquals(SellerType.TRUSTED_SELLER.name(), vendorProductSolr.getSellerType());
  }


  @Test
   void toVendorProductSolr_withForceReviewForPreLiveTest() {
    productReviewer.setApproverAssignee(null);
    product.setPostLive(Boolean.FALSE);
    product.setSellerType(SellerType.NON_TRUSTED_SELLER);
    imageQcProcessedAndBrandResponse.setImageQcProcessedResponse(
      ImageQcProcessedResponse.builder().forceReview(Boolean.TRUE).build());
    product.setCurrentVendor(new Vendor());
    product.getCurrentVendor().setVendorCode(VENDOR_CODE);
    VendorProductSolr vendorProductSolr = VendorProductSolrHelper.toVendorProductSolr(product, productReviewer,
      imageQcProcessedAndBrandResponse);
    Assertions.assertNotNull(vendorProductSolr);
    Assertions.assertEquals(PRODUCT_NAME, vendorProductSolr.getProductName());
    Assertions.assertEquals(PRODUCT_CODE, vendorProductSolr.getProductCode());
    Assertions.assertEquals(SolrConstants.NOT_APPLICABLE,
        vendorProductSolr.getProductApproverAssignee());
    Assertions.assertEquals(VENDOR_CODE, vendorProductSolr.getVendorCode());
    Assertions.assertFalse(vendorProductSolr.isPostLive());
    Assertions.assertEquals(vendorProductSolr.getSellerType(),
        SellerType.NON_TRUSTED_SELLER.name());
  }

  @Test
   void toVendorProductSolr_withForceReviewForPreLiveTrustedSellerTest() {
    productReviewer.setApproverAssignee(null);
    product.setPostLive(Boolean.FALSE);
    product.setSellerType(SellerType.NON_TRUSTED_SELLER);
    imageQcProcessedAndBrandResponse.setImageQcProcessedResponse(
      ImageQcProcessedResponse.builder().forceReview(Boolean.TRUE).build());
    product.setCurrentVendor(new Vendor());
    product.getCurrentVendor().setVendorCode(VENDOR_CODE);
    VendorProductSolr vendorProductSolr = VendorProductSolrHelper.toVendorProductSolr(product, productReviewer,
      imageQcProcessedAndBrandResponse);
    Assertions.assertNotNull(vendorProductSolr);
    Assertions.assertEquals(PRODUCT_NAME, vendorProductSolr.getProductName());
    Assertions.assertEquals(PRODUCT_CODE, vendorProductSolr.getProductCode());
    Assertions.assertEquals(SolrConstants.NOT_APPLICABLE,
        vendorProductSolr.getProductApproverAssignee());
    Assertions.assertEquals(VENDOR_CODE, vendorProductSolr.getVendorCode());
    Assertions.assertFalse(vendorProductSolr.isPostLive());
    Assertions.assertEquals(vendorProductSolr.getSellerType(),
        SellerType.NON_TRUSTED_SELLER.name());
  }

  @Test
   void toVendorProductSolr_withForceReviewFalseForPreLiveTest() {
    productReviewer.setApproverAssignee(null);
    product.setPostLive(Boolean.TRUE);
    imageQcProcessedAndBrandResponse.setImageQcProcessedResponse(
      ImageQcProcessedResponse.builder().forceReview(Boolean.FALSE).build());
    product.setCurrentVendor(new Vendor());
    product.getCurrentVendor().setVendorCode(VENDOR_CODE);
    VendorProductSolr vendorProductSolr = VendorProductSolrHelper.toVendorProductSolr(product, productReviewer,
      imageQcProcessedAndBrandResponse);
    Assertions.assertNotNull(vendorProductSolr);
    Assertions.assertEquals(PRODUCT_NAME, vendorProductSolr.getProductName());
    Assertions.assertEquals(PRODUCT_CODE, vendorProductSolr.getProductCode());
    Assertions.assertEquals(SolrConstants.NOT_APPLICABLE,
        vendorProductSolr.getProductApproverAssignee());
    Assertions.assertEquals(VENDOR_CODE, vendorProductSolr.getVendorCode());
    Assertions.assertTrue(vendorProductSolr.isPostLive());
  }

  @Test
   void toVendorProductSolr_withForceReview_forTrustedSellers() {
    productReviewer.setApproverAssignee(null);
    product.setPostLive(Boolean.TRUE);
    product.setSellerType(SellerType.TRUSTED_SELLER);
    imageQcProcessedAndBrandResponse.setImageQcProcessedResponse(
      ImageQcProcessedResponse.builder().forceReview(Boolean.TRUE).build());
    product.setCurrentVendor(new Vendor());
    product.getCurrentVendor().setVendorCode(VENDOR_CODE);
    VendorProductSolr vendorProductSolr = VendorProductSolrHelper.toVendorProductSolr(product, productReviewer,
      imageQcProcessedAndBrandResponse);
    Assertions.assertNotNull(vendorProductSolr);
    Assertions.assertEquals(PRODUCT_NAME, vendorProductSolr.getProductName());
    Assertions.assertEquals(PRODUCT_CODE, vendorProductSolr.getProductCode());
    Assertions.assertEquals(SolrConstants.NOT_APPLICABLE,
        vendorProductSolr.getProductApproverAssignee());
    Assertions.assertEquals(VENDOR_CODE, vendorProductSolr.getVendorCode());
    Assertions.assertTrue(vendorProductSolr.isPostLive());
  }

  @Test
   void getSolrInputDocumentForClearReviewerDetailsAndUpdateStateTest() {
    SolrInputDocument solrInputDocument = VendorProductSolrHelper
        .getSolrInputDocumentForClearReviewerDetailsAndUpdateState(PRODUCT_CODE,
            WorkflowState.QC_REJECTED);
    Assertions.assertEquals(PRODUCT_CODE,
        solrInputDocument.get(VendorProductSolrFieldNames.PRODUCT_CODE).getValue());
    Assertions.assertEquals(
        Collections.singletonMap(SolrConstants.SET_CLAUSE, WorkflowState.QC_REJECTED.toString()),
        solrInputDocument.get(VendorProductSolrFieldNames.STATE).getFirstValue());
    Assertions.assertEquals(
        Collections.singletonMap(SolrConstants.SET_CLAUSE, SolrConstants.NOT_APPLICABLE),
        solrInputDocument.get(VendorProductSolrFieldNames.APPROVER_ASSIGNEE)
            .getFirstValue());
  }

  @Test
   void getSolrInputDocumentForUpdateStateTest() {
    SolrInputDocument solrInputDocument = VendorProductSolrHelper
        .getSolrInputDocumentForUpdateState(PRODUCT_CODE,
            WorkflowState.QC_REJECTED);
    Assertions.assertEquals(PRODUCT_CODE,
        solrInputDocument.get(VendorProductSolrFieldNames.PRODUCT_CODE).getValue());
    Assertions.assertEquals(
        Collections.singletonMap(SolrConstants.SET_CLAUSE, WorkflowState.QC_REJECTED.toString()),
        solrInputDocument.get(VendorProductSolrFieldNames.STATE).getFirstValue());
  }

  @Test
   void getSolrInputDocumentForUpdateStateAndMfdTrueTest() {
    SolrInputDocument solrInputDocument = VendorProductSolrHelper
        .getSolrInputDocumentForUpdateStateAndMfdTrue(PRODUCT_CODE,
            WorkflowState.QC_REJECTED);
    Assertions.assertEquals(PRODUCT_CODE,
        solrInputDocument.get(VendorProductSolrFieldNames.PRODUCT_CODE).getValue());
    Assertions.assertEquals(
        Collections.singletonMap(SolrConstants.SET_CLAUSE, WorkflowState.QC_REJECTED.toString()),
        solrInputDocument.get(VendorProductSolrFieldNames.STATE).getFirstValue());
    Assertions.assertEquals(Collections.singletonMap(SolrConstants.SET_CLAUSE, true),
        solrInputDocument.get(VendorProductSolrFieldNames.MARK_FOR_DELETE).getFirstValue());
  }

  @Test
   void getInputForPostLiveFlagUpdateTest() {
    SolrInputDocument solrInputDocument =
        VendorProductSolrHelper.getInputForPostLiveFlagUpdate(PRODUCT_CODE, true);
    Assertions.assertEquals(PRODUCT_CODE,
        solrInputDocument.get(VendorProductSolrFieldNames.PRODUCT_CODE).getValue());
    Assertions.assertEquals(Collections.singletonMap(SolrConstants.SET_CLAUSE, true),
        solrInputDocument.get(VendorProductSolrFieldNames.POST_LIVE).getFirstValue());
  }

  @Test
   void getQueryForBusinessPartnerMapperTest() {
    SolrQuery solrQuery =
        VendorProductSolrHelper.getQueryForBusinessPartnerMapper(pendingStates, KEYWORD, pageable, STORE_ID);
    Assertions.assertNotNull(solrQuery);
    Assertions.assertEquals(solrQuery.getFilterQueries().length, 3);
    Assertions.assertTrue(solrQuery.getQuery().contains(pendingStates.get(3)));
    Assertions.assertTrue(solrQuery.getQuery().contains(KEYWORD));
  }

  @Test
   void toProductBusinessPartnerList() {
    ProductBusinessPartnerMapper productBusinessPartnerMapper =
        VendorProductSolrHelper.toProductBusinessPartnerList(vendorSolrInputDocument);
    Assertions.assertEquals(productBusinessPartnerMapper.getBusinessPartnerCode(),
        BUSINESS_PARTNER_CODE);
  }

  @Test
   void getSolrQueryForCountVendorAssignment() {
    SolrQuery solrQuery = VendorProductSolrHelper.getSolrQueryForCountVendorAssignment();
    Assertions.assertNotNull(solrQuery);
    Assertions.assertEquals(solrQuery.getFilterQueries().length, 1);
  }

  @Test
   void getSolrInputDocumentForAutoApprovalAssigneeAndUpdateStateTest() {
    SolrInputDocument solrInputDocument = VendorProductSolrHelper
        .getSolrInputDocumentForAutoApprovalAssigneeAndUpdateState(PRODUCT_CODE,
            WorkflowState.PASSED);
    Assertions.assertEquals(PRODUCT_CODE,
        solrInputDocument.get(VendorProductSolrFieldNames.PRODUCT_CODE).getValue());
    Assertions.assertEquals(
        Collections.singletonMap(SolrConstants.SET_CLAUSE, WorkflowState.PASSED.toString()),
        solrInputDocument.get(VendorProductSolrFieldNames.STATE).getFirstValue());
    Assertions.assertEquals(
        Collections.singletonMap(SolrConstants.SET_CLAUSE, SolrConstants.AUTO_APPROVED),
        solrInputDocument.get(VendorProductSolrFieldNames.APPROVER_ASSIGNEE)
            .getFirstValue());
  }

  @Test
   void getSolrQueryForVendorList_withReviewType3Test() {
    summaryFilterDTO =
      SummaryFilterDTO.builder().timeFilterType(TimeFilterType.ALL).vendorCode(VENDOR_CODE)
        .contentPending(Boolean.TRUE).imagePending(Boolean.TRUE).build();
    SolrQuery solrQuery = VendorProductSolrHelper
      .getSolrQueryForVendorList(STORE_ID, summaryFilterDTO, workflowStateList, pageable);
    Assertions.assertTrue(Arrays.asList(solrQuery.getFilterQueries()).contains(
      VendorProductSolrFieldNames.REVIEW_TYPE + SolrConstants.COLON
        + ReviewType.CONTENT_AND_IMAGE.getValue()));
  }

  @Test
   void getSolrQueryForVendorList_withReviewType1Test() {
    summaryFilterDTO =
      SummaryFilterDTO.builder().timeFilterType(TimeFilterType.ALL).vendorCode(VENDOR_CODE)
        .contentPending(Boolean.TRUE).imagePending(Boolean.FALSE).build();
    SolrQuery solrQuery = VendorProductSolrHelper
      .getSolrQueryForVendorList(STORE_ID, summaryFilterDTO, workflowStateList, pageable);
    Assertions.assertTrue(Arrays.asList(solrQuery.getFilterQueries()).contains(
      VendorProductSolrFieldNames.REVIEW_TYPE + SolrConstants.COLON
        + ReviewType.CONTENT.getValue()));
  }

  @Test
   void getSolrQueryForVendorList_withReviewType2Test() {
    summaryFilterDTO =
      SummaryFilterDTO.builder().timeFilterType(TimeFilterType.ALL).vendorCode(VENDOR_CODE)
        .contentPending(Boolean.FALSE).imagePending(Boolean.TRUE).build();
    SolrQuery solrQuery = VendorProductSolrHelper
      .getSolrQueryForVendorList(STORE_ID, summaryFilterDTO, workflowStateList, pageable);
    Assertions.assertTrue(Arrays.asList(solrQuery.getFilterQueries()).contains(
      VendorProductSolrFieldNames.REVIEW_TYPE + SolrConstants.COLON
        + ReviewType.IMAGE.getValue()));
  }

  @Test
   void getSolrQueryForVendorList_sellerTypeTest() {
    summaryFilterDTO = SummaryFilterDTO.builder().timeFilterType(TimeFilterType.ALL).vendorCode(VENDOR_CODE)
        .businessPartnerCode(SolrConstants.TRUSTED_SELLER).build();
    SolrQuery solrQuery =
        VendorProductSolrHelper.getSolrQueryForVendorList(STORE_ID, summaryFilterDTO, workflowStateList, pageable);
    Assertions.assertTrue(Arrays.asList(solrQuery.getFilterQueries())
        .contains(VendorProductSolrFieldNames.SELLER_TYPE + SolrConstants.COLON + "1"));
  }

  @Test
   void getSolrQueryForVendorList_editedTest() {
    summaryFilterDTO =
      SummaryFilterDTO.builder().edited(true).revised(false).timeFilterType(TimeFilterType.ALL)
      .vendorCode(VENDOR_CODE).build();
    SolrQuery solrQuery =
      VendorProductSolrHelper.getSolrQueryForVendorList(STORE_ID, summaryFilterDTO,
        workflowStateList, pageable);
    Assertions.assertNotNull(solrQuery.getQuery());
    Assertions.assertTrue(Arrays.asList(solrQuery.getFilterQueries()).contains(
      VendorProductSolrFieldNames.PRODUCT_REVIEW_TYPE + SolrConstants.COLON
        + ProductReviewType.EDITED.getValue()));
  }

  @Test
   void getSolrQueryForVendorList_newlyAddedTest() {
    //multiselect (only newly Added)
    summaryFilterDTO =
      SummaryFilterDTO.builder().edited(false).revised(false).timeFilterType(TimeFilterType.ALL)
        .vendorCode(VENDOR_CODE).build();
    SolrQuery solrQuery =
      VendorProductSolrHelper.getSolrQueryForVendorList(STORE_ID, summaryFilterDTO,
        workflowStateList, pageable);
    Assertions.assertNotNull(solrQuery.getQuery());
    Assertions.assertTrue(Arrays.asList(solrQuery.getFilterQueries()).contains(
      VendorProductSolrFieldNames.PRODUCT_REVIEW_TYPE + SolrConstants.COLON
        + ProductReviewType.NEWLY_ADDED.getValue()));
  }

  @Test
   void getSolrQueryForVendorList_EditedAndNewlyAdded() {
    //multiselect (edited and newly added)
    summaryFilterDTO = SummaryFilterDTO.builder().edited(null).revised(false).postLive(false)
      .timeFilterType(TimeFilterType.ALL).vendorCode(VENDOR_CODE).build();
    SolrQuery solrQuery =
      VendorProductSolrHelper.getSolrQueryForVendorList(STORE_ID, summaryFilterDTO,
        workflowStateList, pageable);
    Assertions.assertNotNull(solrQuery.getQuery());
    Assertions.assertTrue(Arrays.asList(solrQuery.getFilterQueries()).contains(
      VendorProductSolrFieldNames.PRODUCT_REVIEW_TYPE + SolrConstants.COLON
        + SolrConstants.OPEN_BRACKET + String.join(SolrConstants.OR,
        ProductReviewType.NEWLY_ADDED.getValue(), ProductReviewType.EDITED.getValue())
        + SolrConstants.CLOSE_BRACKET));
  }

  @Test
   void getSolrQueryForVendorList_RevisedAndNewlyAdded() {
    //multiselect (Revised and newly added)
    summaryFilterDTO =
      SummaryFilterDTO.builder().edited(false).revised(null).postLive(true).timeFilterType(TimeFilterType.ALL)
        .vendorCode(VENDOR_CODE).build();
    SolrQuery solrQuery =
      VendorProductSolrHelper.getSolrQueryForVendorList(STORE_ID, summaryFilterDTO,
        workflowStateList, pageable);
    Assertions.assertNotNull(solrQuery.getQuery());
    Assertions.assertTrue(Arrays.asList(solrQuery.getFilterQueries()).contains(
      VendorProductSolrFieldNames.PRODUCT_REVIEW_TYPE + SolrConstants.COLON
        + SolrConstants.OPEN_BRACKET + String.join(SolrConstants.OR,
        ProductReviewType.NEWLY_ADDED.getValue(), ProductReviewType.REVISED.getValue())
        + SolrConstants.CLOSE_BRACKET));
  }

  @Test
   void getSolrQueryForVendorPostLiveNullTest() {
    summaryFilterDTO =
      SummaryFilterDTO.builder().postLive(null).timeFilterType(TimeFilterType.ALL)
        .vendorCode(VENDOR_CODE).build();
    SolrQuery solrQuery =
      VendorProductSolrHelper.getSolrQueryForVendorList(STORE_ID, summaryFilterDTO,
        workflowStateList, pageable);
    Assertions.assertNotNull(solrQuery.getQuery());
    System.out.println(solrQuery);
    Assertions.assertFalse(Arrays.asList(solrQuery.getFilterQueries()).contains(
      VendorProductSolrFieldNames.POST_LIVE));
  }

  @Test
   void getSolrQueryForVendorListRevisedAndEditedTest() {
    summaryFilterDTO =
      SummaryFilterDTO.builder().edited(true).revised(true).timeFilterType(TimeFilterType.ALL)
        .vendorCode(VENDOR_CODE).build();
    SolrQuery solrQuery =
      VendorProductSolrHelper.getSolrQueryForVendorList(STORE_ID, summaryFilterDTO,
        workflowStateList, pageable);
    Assertions.assertNotNull(solrQuery.getQuery());
    Assertions.assertTrue(Arrays.asList(solrQuery.getFilterQueries()).contains(
      VendorProductSolrFieldNames.PRODUCT_REVIEW_TYPE + SolrConstants.COLON
        + SolrConstants.OPEN_BRACKET + String.join(SolrConstants.OR,
        ImmutableSet.of(ProductReviewType.REVISED.getValue(),
          ProductReviewType.EDITED.getValue())) + SolrConstants.CLOSE_BRACKET));
  }
  @Test
   void getSolrQueryForVendorListRevisedAndEditedFalseTest() {
    summaryFilterDTO =
      SummaryFilterDTO.builder().edited(true).revised(null).timeFilterType(TimeFilterType.ALL)
        .vendorCode(VENDOR_CODE).build();
    SolrQuery solrQuery =
      VendorProductSolrHelper.getSolrQueryForVendorList(STORE_ID, summaryFilterDTO,
        workflowStateList, pageable);
    Assertions.assertNotNull(solrQuery.getQuery());
    System.out.println(solrQuery);
    Assertions.assertFalse(Arrays.asList(solrQuery.getFilterQueries()).contains(
      VendorProductSolrFieldNames.PRODUCT_REVIEW_TYPE));
  }

  @Test
   void getSolrQueryForVendorListAllSelectionsTest() {
    summaryFilterDTO =
      SummaryFilterDTO.builder().edited(null).revised(true).timeFilterType(TimeFilterType.ALL)
        .vendorCode(VENDOR_CODE).build();
    SolrQuery solrQuery =
      VendorProductSolrHelper.getSolrQueryForVendorList(STORE_ID, summaryFilterDTO,
        workflowStateList, pageable);
    Assertions.assertNotNull(solrQuery.getQuery());
    System.out.println(solrQuery);
    Assertions.assertFalse(Arrays.asList(solrQuery.getFilterQueries()).contains(
      VendorProductSolrFieldNames.PRODUCT_REVIEW_TYPE));
  }


  @Test
   void getSolrQueryForVendorListNoneSelectedTest() {
    summaryFilterDTO =
      SummaryFilterDTO.builder().edited(null).revised(null).timeFilterType(TimeFilterType.ALL)
        .vendorCode(VENDOR_CODE).build();
    SolrQuery solrQuery =
      VendorProductSolrHelper.getSolrQueryForVendorList(STORE_ID, summaryFilterDTO,
        workflowStateList, pageable);
    Assertions.assertNotNull(solrQuery.getQuery());
    Assertions.assertTrue(Arrays.asList(solrQuery.getFilterQueries()).contains(
      VendorProductSolrFieldNames.PRODUCT_REVIEW_TYPE + SolrConstants.COLON
        + SolrConstants.OPEN_BRACKET + String.join(SolrConstants.OR,
        ProductReviewType.NEWLY_ADDED.getValue(), ProductReviewType.REVISED.getValue(),
        ProductReviewType.EDITED.getValue()) + SolrConstants.CLOSE_BRACKET));
  }

  @Test
   void getSolrQueryForVendorList_revisedTest() {
    summaryFilterDTO =
      SummaryFilterDTO.builder().revised(true).edited(false).timeFilterType(TimeFilterType.ALL)
      .vendorCode(VENDOR_CODE).build();
    SolrQuery solrQuery =
      VendorProductSolrHelper.getSolrQueryForVendorList(STORE_ID, summaryFilterDTO,
        workflowStateList, pageable);
    Assertions.assertNotNull(solrQuery.getQuery());
    Assertions.assertTrue(Arrays.asList(solrQuery.getFilterQueries()).contains(
      VendorProductSolrFieldNames.PRODUCT_REVIEW_TYPE + SolrConstants.COLON
        + ProductReviewType.REVISED.getValue()));
  }

  @Test
   void getSolrInputDocumentForProductRetryUpdateTest() {
    SolrInputDocument solrInputDocument =
      VendorProductSolrHelper.getSolrInputDocumentForProductRetryUpdate(product);
    Assertions.assertNotNull(solrInputDocument);
    Assertions.assertEquals(PRODUCT_CODE,
        solrInputDocument.getField(VendorProductSolrFieldNames.PRODUCT_CODE).getValue());
    Assertions.assertEquals(WorkflowState.IN_REVIEW.toString(),
        ((Map) solrInputDocument.getField(VendorProductSolrFieldNames.STATE).getValue()).get(
          SolrConstants.SET_CLAUSE));
    Assertions.assertEquals(ReviewType.CONTENT.getValue(),
        ((Map) solrInputDocument.getField(VendorProductSolrFieldNames.REVIEW_TYPE).getValue()).get(
          SolrConstants.SET_CLAUSE));
  }

  @Test
   void getSolrQueryForFilteredBoostedProductsTest() {
    summaryFilterDTO.setPostLive(true);
    summaryFilterDTO.setBusinessPartnerCode(SolrConstants.EXTERNAL);
    summaryFilterDTO.setEdited(true);
    summaryFilterDTO.setRevised(false);
    summaryFilterDTO.setBrandPending(true);
    summaryFilterDTO.setContentPending(true);
    summaryFilterDTO.setImagePending(false);
    summaryFilterDTO.setTimeFilterType(TimeFilterType.TODAY);
    summaryFilterDTO.setRestrictedKeyword(true);
    summaryFilterDTO.setFaultyImageType("BLUR");
    SolrQuery mockedQuery = Mockito.spy(boostQueryForAutoAssignment);
    SolrQuery result = VendorProductSolrHelper.getSolrQueryForFilteredBoostedProducts(summaryFilterDTO, pageable, STORE_ID, mockedQuery);
    Assertions.assertEquals(SolrConstants.JKT_TIME_ZONE, result.get(CommonParams.TZ));
    Assertions.assertEquals(10, result.getFilterQueries().length);
    Assertions.assertEquals(VendorProductSolrFieldNames.POST_LIVE + SolrConstants.COLON + true,
        result.getFilterQueries()[0]);
    Assertions.assertEquals(
        VendorProductSolrFieldNames.PRODUCT_REVIEW_TYPE + SolrConstants.COLON + "1",
        result.getFilterQueries()[1]);
    Assertions.assertEquals(
        VendorProductSolrFieldNames.BRAND_APPROVAL_STATUS + SolrConstants.COLON + BRAND_APPROVAL,
        result.getFilterQueries()[2]);
    Assertions.assertEquals(
        VendorProductSolrFieldNames.MARK_FOR_DELETE + SolrConstants.COLON + false,
        result.getFilterQueries()[3]);
    Assertions.assertEquals(VendorProductSolrFieldNames.REVIEW_TYPE + SolrConstants.COLON + "1",
        result.getFilterQueries()[4]);
    Assertions.assertEquals(VendorProductSolrFieldNames.STORE_ID + SolrConstants.COLON + STORE_ID,
        result.getFilterQueries()[5]);
    Assertions.assertEquals(
        VendorProductSolrFieldNames.PRODUCT_CREATED_DATE + SolrConstants.COLON + PRODUCT_CREATED_DATE,
        result.getFilterQueries()[6]);
  }

  @Test
   void getSolrQueryForFilteredBoostedProductsTestForFaultyImage() {
    summaryFilterDTO.setPostLive(true);
    summaryFilterDTO.setEdited(true);
    summaryFilterDTO.setRevised(false);
    summaryFilterDTO.setBrandPending(true);
    summaryFilterDTO.setContentPending(true);
    summaryFilterDTO.setImagePending(false);
    summaryFilterDTO.setTimeFilterType(TimeFilterType.TODAY);
    summaryFilterDTO.setRestrictedKeyword(true);
    summaryFilterDTO.setBusinessPartnerCode(SolrConstants.TRUSTED_SELLER);
    summaryFilterDTO.setFaultyImageType(ProductLabels.GOOD.getDescription());
    SolrQuery mockedQuery = Mockito.spy(boostQueryForAutoAssignment);
    SolrQuery result = VendorProductSolrHelper.getSolrQueryForFilteredBoostedProducts(summaryFilterDTO, pageable, STORE_ID, mockedQuery);
    Assertions.assertEquals(SolrConstants.JKT_TIME_ZONE, result.get(CommonParams.TZ));
    Assertions.assertEquals(10, result.getFilterQueries().length);
    Assertions.assertEquals(VendorProductSolrFieldNames.POST_LIVE + SolrConstants.COLON + true,
        result.getFilterQueries()[0]);
    Assertions.assertEquals(
        VendorProductSolrFieldNames.PRODUCT_REVIEW_TYPE + SolrConstants.COLON + "1",
        result.getFilterQueries()[1]);
    Assertions.assertEquals(
        VendorProductSolrFieldNames.BRAND_APPROVAL_STATUS + SolrConstants.COLON + BRAND_APPROVAL,
        result.getFilterQueries()[2]);
    Assertions.assertEquals(
        VendorProductSolrFieldNames.MARK_FOR_DELETE + SolrConstants.COLON + false,
        result.getFilterQueries()[3]);
    Assertions.assertEquals(VendorProductSolrFieldNames.REVIEW_TYPE + SolrConstants.COLON + "1",
        result.getFilterQueries()[4]);
    Assertions.assertEquals(VendorProductSolrFieldNames.STORE_ID + SolrConstants.COLON + STORE_ID,
        result.getFilterQueries()[5]);
    Assertions.assertEquals(
        VendorProductSolrFieldNames.PRODUCT_CREATED_DATE + SolrConstants.COLON + PRODUCT_CREATED_DATE,
        result.getFilterQueries()[6]);
  }

  @Test
   void getSolrQueryForFilteredBoostedProductsTestForBusinessPartnerCode() {
    summaryFilterDTO.setPostLive(true);
    summaryFilterDTO.setEdited(true);
    summaryFilterDTO.setRevised(false);
    summaryFilterDTO.setBrandPending(true);
    summaryFilterDTO.setContentPending(true);
    summaryFilterDTO.setImagePending(false);
    summaryFilterDTO.setTimeFilterType(TimeFilterType.TODAY);
    summaryFilterDTO.setRestrictedKeyword(true);
    summaryFilterDTO.setFaultyImageType(ProductLabels.GOOD.getDescription());
    summaryFilterDTO.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    SolrQuery mockedQuery = Mockito.spy(boostQueryForAutoAssignment);
    SolrQuery result = VendorProductSolrHelper.getSolrQueryForFilteredBoostedProducts(summaryFilterDTO, pageable, STORE_ID, mockedQuery);
    Assertions.assertEquals(SolrConstants.JKT_TIME_ZONE, result.get(CommonParams.TZ));
    Assertions.assertEquals(10, result.getFilterQueries().length);
    Assertions.assertEquals(VendorProductSolrFieldNames.POST_LIVE + SolrConstants.COLON + true,
        result.getFilterQueries()[0]);
    Assertions.assertEquals(
        VendorProductSolrFieldNames.PRODUCT_REVIEW_TYPE + SolrConstants.COLON + "1",
        result.getFilterQueries()[1]);
    Assertions.assertEquals(
        VendorProductSolrFieldNames.BRAND_APPROVAL_STATUS + SolrConstants.COLON + BRAND_APPROVAL,
        result.getFilterQueries()[2]);
    Assertions.assertEquals(
        VendorProductSolrFieldNames.MARK_FOR_DELETE + SolrConstants.COLON + false,
        result.getFilterQueries()[3]);
    Assertions.assertEquals(VendorProductSolrFieldNames.REVIEW_TYPE + SolrConstants.COLON + "1",
        result.getFilterQueries()[4]);
    Assertions.assertEquals(VendorProductSolrFieldNames.STORE_ID + SolrConstants.COLON + STORE_ID,
        result.getFilterQueries()[5]);
    Assertions.assertEquals(
        VendorProductSolrFieldNames.PRODUCT_CREATED_DATE + SolrConstants.COLON + PRODUCT_CREATED_DATE,
        result.getFilterQueries()[6]);
  }

  @Test
   void getSolrQueryForFilteredBoostedPostLiveNullProductsTest() {
    summaryFilterDTO.setPostLive(null);
    summaryFilterDTO.setEdited(false);
    summaryFilterDTO.setRevised(false);
    summaryFilterDTO.setBrandPending(true);
    summaryFilterDTO.setContentPending(true);
    summaryFilterDTO.setImagePending(false);
    summaryFilterDTO.setTimeFilterType(TimeFilterType.TODAY);
    summaryFilterDTO.setRestrictedKeyword(true);
    summaryFilterDTO.setFaultyImageType("BLUR");
    SolrQuery mockedQuery = Mockito.spy(boostQueryForAutoAssignment);
    SolrQuery result = VendorProductSolrHelper.getSolrQueryForFilteredBoostedProducts(summaryFilterDTO, pageable, STORE_ID, mockedQuery);
    Assertions.assertEquals(SolrConstants.JKT_TIME_ZONE, result.get(CommonParams.TZ));
    Assertions.assertEquals(9, result.getFilterQueries().length);
    Assertions.assertEquals(
        VendorProductSolrFieldNames.BRAND_APPROVAL_STATUS + SolrConstants.COLON + BRAND_APPROVAL,
        result.getFilterQueries()[2]);
    Assertions.assertEquals(
        VendorProductSolrFieldNames.MARK_FOR_DELETE + SolrConstants.COLON + false,
        result.getFilterQueries()[3]);
    Assertions.assertEquals(VendorProductSolrFieldNames.REVIEW_TYPE + SolrConstants.COLON + "1",
        result.getFilterQueries()[4]);
    Assertions.assertEquals(VendorProductSolrFieldNames.STORE_ID + SolrConstants.COLON + STORE_ID,
        result.getFilterQueries()[5]);
    Assertions.assertEquals(
        VendorProductSolrFieldNames.PRODUCT_CREATED_DATE + SolrConstants.COLON + PRODUCT_CREATED_DATE,
        result.getFilterQueries()[6]);
  }

  @Test
   void getSolrQueryForFilteredBoostedEmptyReviewProductsTest() {
    SolrQuery mockedQuery = Mockito.spy(boostQueryForAutoAssignment);
    summaryFilterDTO =
        SummaryFilterDTO.builder().edited(true).revised(null).timeFilterType(TimeFilterType.ALL).vendorCode(VENDOR_CODE)
            .build();
    SolrQuery solrQuery =
        VendorProductSolrHelper.getSolrQueryForFilteredBoostedProducts(summaryFilterDTO, pageable, STORE_ID,
            mockedQuery);
    Assertions.assertFalse(
        Arrays.asList(solrQuery.getFilterQueries()).contains(VendorProductSolrFieldNames.PRODUCT_REVIEW_TYPE));
  }

  @Test
   void getSolrQueryForFilteredBoostedBrandPendingProductsTest() {
    summaryFilterDTO.setPostLive(null);
    summaryFilterDTO.setEdited(false);
    summaryFilterDTO.setRevised(false);
    summaryFilterDTO.setBrandPending(null);
    summaryFilterDTO.setContentPending(null);
    summaryFilterDTO.setImagePending(false);
    summaryFilterDTO.setTimeFilterType(null);
    summaryFilterDTO.setRestrictedKeyword(true);
    SolrQuery mockedQuery = Mockito.spy(boostQueryForAutoAssignment);
    SolrQuery result = VendorProductSolrHelper.getSolrQueryForFilteredBoostedProducts(summaryFilterDTO, pageable, STORE_ID, mockedQuery);
    Assertions.assertEquals(SolrConstants.JKT_TIME_ZONE, result.get(CommonParams.TZ));
    Assertions.assertEquals(6, result.getFilterQueries().length);
  }

  @Test
   void toProductUpdateProductToSolrEventModelForBrandUpdateTest(){
    product.setState(null);
    PDTProductUpdateProductToSolrEventModel pdtProductUpdateProductToSolrEventModel =
        VendorProductSolrHelper.toProductUpdateProductToSolrEventModelForBrandUpdate(product);
    Assertions.assertEquals(pdtProductUpdateProductToSolrEventModel.getProductCode(),
        product.getProductCode());
    Assertions.assertEquals(pdtProductUpdateProductToSolrEventModel.getProductName(),
        product.getProductName());
    Assertions.assertEquals(pdtProductUpdateProductToSolrEventModel.getBrand(), product.getBrand());
  }

  @Test
   void toProductUpdateProductToSolrEventModelForBrandUpdateWithSellerBadgeTest(){
    product.setSellerBadge(SellerBadge.BRONZE_MERCHANT);
    product.setSellerType(SellerType.TRUSTED_SELLER);
    PDTProductUpdateProductToSolrEventModel pdtProductUpdateProductToSolrEventModel =
        VendorProductSolrHelper.toProductUpdateProductToSolrEventModelForBrandUpdate(product);
    Assertions.assertEquals(pdtProductUpdateProductToSolrEventModel.getProductCode(),
        product.getProductCode());
    Assertions.assertEquals(pdtProductUpdateProductToSolrEventModel.getProductName(),
        product.getProductName());
    Assertions.assertEquals(pdtProductUpdateProductToSolrEventModel.getBrand(), product.getBrand());
  }

  @Test
  public void getSolrQueryForIprListingTest() {
    IPRProductListRequest iprProductListRequest = new IPRProductListRequest();
    iprProductListRequest.setBrandCode(BRAND);
    iprProductListRequest.setAssignedTo(ASSIGNEE_EMAIL);
    iprProductListRequest.setKeyword(PRODUCT_CODE);
    iprProductListRequest.setState(ProductStateIPR.IN_REVIEW.name());
    iprProductListRequest.setTimeFilterWebType(TimeFilterType.TODAY.getTimeFilterType());
    iprProductListRequest.setCategoryCode(CATEGORY_CODE);
    iprProductListRequest.setSortOrder(ORDER_BY_DESC);
    iprProductListRequest.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    SolrQuery solrQuery =
      VendorProductSolrHelper.getSolrQueryForIprProductList(STORE_ID, iprProductListRequest,
        pageable);
    Assertions.assertNotNull(solrQuery.getQuery());
  }

  @Test
  public void getSolrQueryForIprListingTimeFilterAllTest() {
    IPRProductListRequest iprProductListRequest = new IPRProductListRequest();
    iprProductListRequest.setBrandCode(BRAND);
    iprProductListRequest.setAssignedTo(ASSIGNEE_EMAIL);
    iprProductListRequest.setKeyword(PRODUCT_CODE);
    iprProductListRequest.setState(ProductStateIPR.IN_REVIEW.name());
    iprProductListRequest.setTimeFilterWebType(TimeFilterType.ALL.getTimeFilterType());
    iprProductListRequest.setCategoryCode(CATEGORY_CODE);
    iprProductListRequest.setSortOrder(ORDER_BY_DESC);
    iprProductListRequest.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    iprProductListRequest.setAssigned(Boolean.TRUE);
    SolrQuery solrQuery =
      VendorProductSolrHelper.getSolrQueryForIprProductList(STORE_ID, iprProductListRequest,
        pageable);
    Assertions.assertNotNull(solrQuery.getQuery());
  }

  @Test
  void getSolrQueryForIprListingNotAssignedTest() {
    IPRProductListRequest iprProductListRequest = new IPRProductListRequest();
    iprProductListRequest.setBrandCode(BRAND);
    iprProductListRequest.setAssignedTo(ASSIGNEE_EMAIL);
    iprProductListRequest.setKeyword(PRODUCT_CODE);
    iprProductListRequest.setState(ProductStateIPR.IN_REVIEW.name());
    iprProductListRequest.setTimeFilterWebType(TimeFilterType.ALL.getTimeFilterType());
    iprProductListRequest.setCategoryCode(CATEGORY_CODE);
    iprProductListRequest.setSortOrder(ORDER_BY_DESC);
    iprProductListRequest.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    iprProductListRequest.setAssigned(Boolean.FALSE);
    SolrQuery solrQuery =
      VendorProductSolrHelper.getSolrQueryForIprProductList(STORE_ID, iprProductListRequest,
        pageable);
    Assertions.assertNotNull(solrQuery.getQuery());
  }

  @Test
  public void getSolrIprProductCollectionTest() {
    vendorSolrInputDocument.setField(IprProductSolrFieldNames.STATE,
      ProductStateIPR.IN_REVIEW.getValue());
    vendorSolrInputDocument.setField(IprProductSolrFieldNames.CATEGORY_CODE, CATEGORY_CODE);
    vendorSolrInputDocument.setField(IprProductSolrFieldNames.CATEGORY_NAME, CATEGORY_NAME);
    vendorSolrInputDocument.setField(IprProductSolrFieldNames.BRAND_NAME, BRAND);
    vendorSolrInputDocument.setField(IprProductSolrFieldNames.BRAND_CODE, BRAND);
    vendorSolrInputDocument.setField(IprProductSolrFieldNames.PRODUCT_SKU, PRODUCT_CODE);
    vendorSolrInputDocument.setField(IprProductSolrFieldNames.PRODUCT_ADDED_DATE, new Date());
    vendorSolrInputDocument.setField(IprProductSolrFieldNames.ASSIGNED_DATE, new Date());
    vendorSolrInputDocument.setField(IprProductSolrFieldNames.ASSIGNED_TO, ASSIGNEE_EMAIL);
    vendorSolrInputDocument.setField(IprProductSolrFieldNames.SOURCE, SOURCE);
    IPRProductSolr iprProductSolr =
      VendorProductSolrHelper.getSolrIprProductCollection(vendorSolrInputDocument);
    Assertions.assertEquals(PRODUCT_CODE, iprProductSolr.getProductCode());
    Assertions.assertEquals(PRODUCT_NAME, iprProductSolr.getProductName());
    Assertions.assertEquals(CATEGORY_CODE, iprProductSolr.getCategoryCode());
    Assertions.assertEquals(SOURCE, iprProductSolr.getSource());
  }
}
