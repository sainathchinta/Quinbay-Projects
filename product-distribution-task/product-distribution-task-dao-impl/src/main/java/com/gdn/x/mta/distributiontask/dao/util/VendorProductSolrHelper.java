package com.gdn.x.mta.distributiontask.dao.util;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import com.gdn.x.mta.distributiontask.model.dto.IPRProductListRequest;
import com.gdn.x.mta.distributiontask.model.solr.IprProductSolrFieldNames;
import com.gdn.x.mta.distributiontask.model.enums.ProductStateIPR;
import com.gdn.x.mta.distributiontask.model.solr.IPRProductSolr;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.EnumUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.solr.client.solrj.SolrQuery;
import org.apache.solr.client.solrj.util.ClientUtils;
import org.apache.solr.common.SolrDocument;
import org.apache.solr.common.SolrInputDocument;
import org.apache.solr.common.params.CommonParams;
import org.springframework.beans.BeanUtils;
import org.springframework.data.domain.Pageable;

import com.gda.mta.product.dto.ImageQcProcessedAndBrandResponse;
import com.gdn.x.mta.distributiontask.domain.event.model.PDTProductSolrAddDomainEventModel;
import com.gdn.x.mta.distributiontask.domain.event.model.PDTProductUpdateProductToSolrEventModel;
import com.gdn.x.mta.distributiontask.model.Constants;
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
import lombok.extern.slf4j.Slf4j;

@Slf4j
public class VendorProductSolrHelper {

  private static final String REGEX_FOR_SPACE = "\\s+";
  private static final String REGEX_FOR_SPECIAL_CHARACTERS = "[^A-Za-z0-9]";
  private static final Pattern PATTERN_FOR_SPECIAL_CHARACTERS = Pattern.compile(REGEX_FOR_SPECIAL_CHARACTERS);
  private static final String BRAND_APPROVED_STATE = "APPROVED";
  private static final String GOOD_EN = "Good";
  private static final String ORDER_BY_DESC = "desc";
  private static final String CONTENT = "content";
  private static final String IMAGE = "image";
  private static final String CONTENT_APPROVER_ASSIGNEE_IMAGE_APPROVER_ASSIGNEE =
      "contentApproverAssignee,imageApproverAssignee";
  public static final String VENDOR_CODE_STATE = "vendorCode,state";

  public static String getQueryForDeleteAll() {
    return SolrConstants.QUERY_PRODUCT_CODE + SolrConstants.LIKE_QUERY;
  }

  public static String getIprQueryForDelete(String productSku) {
    return SolrConstants.QUERY_PRODUCT_SKU + productSku;
  }

  public static List<PDTProductSolrAddDomainEventModel> toPDTProductSolrAddDomainEventModelList(
      List<ProductAndReviewerDetailsDTO> productAndReviewerDetailsDTOList) throws Exception {
    List<PDTProductSolrAddDomainEventModel> pdtProductSolrAddDomainEventModelList =
        new ArrayList<>();
    for (ProductAndReviewerDetailsDTO productAndReviewerDetailsDTO : productAndReviewerDetailsDTOList) {
      pdtProductSolrAddDomainEventModelList.add(toPDTProductSolrAddDomainEventModel(productAndReviewerDetailsDTO));
    }
    return pdtProductSolrAddDomainEventModelList;
  }

  public static PDTProductSolrAddDomainEventModel toPDTProductSolrAddDomainEventModel(
      ProductAndReviewerDetailsDTO productAndReviewerDetailsDTO) throws Exception {
    Product product = productAndReviewerDetailsDTO.getProduct();
    List<String> productChannel = new ArrayList<>();
    PDTProductSolrAddDomainEventModel productSolrAddDomainEventModel =
        new PDTProductSolrAddDomainEventModel();
    BeanUtils.copyProperties(product, productSolrAddDomainEventModel, "rejectedCount", "state", "sellerType",
        "productChannel");
    productSolrAddDomainEventModel.setState(
        Objects.nonNull(product.getState()) ? product.getState().toString() : StringUtils.EMPTY);
    productSolrAddDomainEventModel.setRejectedCount(
        Objects.nonNull(product.getRejectedCount()) ? product.getRejectedCount() : 0);
    productSolrAddDomainEventModel.setCategoryCodes(Arrays.asList(product.getCategoryCode()));
    productSolrAddDomainEventModel.setCategoryNames(Arrays.asList(product.getCategoryName()));
    productSolrAddDomainEventModel.setVendorCode(Objects.nonNull(product.getCurrentVendor()) ?
        product.getCurrentVendor().getVendorCode() :
        SolrConstants.NOT_APPLICABLE);
    productSolrAddDomainEventModel.setProductApprovedDate(
      productAndReviewerDetailsDTO.getProductReviewer().getApprovedDate());
    productSolrAddDomainEventModel.setProductAssignedDate(
      productAndReviewerDetailsDTO.getProductReviewer().getAssignedDate());
    productSolrAddDomainEventModel.setProductApproverAssignee(
      productAndReviewerDetailsDTO.getProductReviewer().getApproverAssignee());
    productSolrAddDomainEventModel.setRestrictedKeywordsPresent(product.isRestrictedKeywordsPresent());
    productSolrAddDomainEventModel.setReviewType(Objects.nonNull(product.getReviewType()) ?
      product.getReviewType().name() : StringUtils.EMPTY);
    productSolrAddDomainEventModel.setRevised(product.isRevised());
    productSolrAddDomainEventModel.setAppealedProduct(product.isAppealedProduct());
    productSolrAddDomainEventModel.setImageViolations(setImageViolations(product));
    productSolrAddDomainEventModel.setPostLive(validateAndGetPostLiveStatus(product));
    if (Objects.nonNull(product.getSellerType())) {
      productSolrAddDomainEventModel.setSellerType(product.getSellerType().getValue());
    }
    if(Objects.nonNull(product.getSellerBadge())){
      productSolrAddDomainEventModel.setSellerBadge(product.getSellerBadge().name());
    }
    if (Boolean.TRUE.equals(product.isB2bActivated())) {
      productChannel.add(Constants.B2B);
    }
    if (Boolean.TRUE.equals(product.isB2cActivated())) {
      productChannel.add(Constants.RETAIL);
    }
    productSolrAddDomainEventModel.setProductChannel(productChannel);
    productSolrAddDomainEventModel.setDistributionMappingStatus(
        product.getDistributionMappingStatus());
    productSolrAddDomainEventModel.setProductCreationType(product.getProductCreationType());
    return productSolrAddDomainEventModel;
  }

  public static PDTProductUpdateProductToSolrEventModel toProductUpdateProductToSolrEventModel(Product product,
      ProductReviewer productReviewer) {
    PDTProductUpdateProductToSolrEventModel productAddProductToSolrEventModel =
        new PDTProductUpdateProductToSolrEventModel();
    BeanUtils.copyProperties(product, productAddProductToSolrEventModel, "sellerType");
    productAddProductToSolrEventModel.setState(
        Objects.nonNull(product.getState()) ? product.getState().toString() : StringUtils.EMPTY);
    productAddProductToSolrEventModel.setCategoryCodes(Arrays.asList(product.getCategoryCode()));
    productAddProductToSolrEventModel.setCategoryNames(Arrays.asList(product.getCategoryName()));
    productAddProductToSolrEventModel.setApprovedDate(productReviewer.getApprovedDate());
    productAddProductToSolrEventModel.setReviewType(product.getReviewType().getValue());
    productAddProductToSolrEventModel.setPostLive(validateAndGetPostLiveStatus(product));
    productAddProductToSolrEventModel.setAppealedProduct(product.isAppealedProduct());
    if (Objects.nonNull(product.getSellerType())) {
      productAddProductToSolrEventModel.setSellerType(product.getSellerType().getValue());
    }
    if(Objects.nonNull(product.getSellerBadge())){
      productAddProductToSolrEventModel.setSellerBadge(product.getSellerBadge().name());
    }
    return productAddProductToSolrEventModel;
  }

  public static PDTProductUpdateProductToSolrEventModel toProductUpdateProductToSolrEventModelForBrandUpdate(
      Product product) {
    PDTProductUpdateProductToSolrEventModel productAddProductToSolrEventModel =
        new PDTProductUpdateProductToSolrEventModel();
    BeanUtils.copyProperties(product, productAddProductToSolrEventModel, "sellerType");
    productAddProductToSolrEventModel
        .setState(Objects.nonNull(product.getState()) ? product.getState().toString() : StringUtils.EMPTY);
    productAddProductToSolrEventModel.setCategoryCodes(Collections.singletonList(product.getCategoryCode()));
    productAddProductToSolrEventModel.setCategoryNames(Collections.singletonList(product.getCategoryName()));
    productAddProductToSolrEventModel.setReviewType(product.getReviewType().getValue());
    productAddProductToSolrEventModel.setPostLive(validateAndGetPostLiveStatus(product));
    productAddProductToSolrEventModel.setUpdatedDate(new Date());
    if (Objects.nonNull(product.getSellerType())) {
      productAddProductToSolrEventModel.setSellerType(product.getSellerType().getValue());
    }
    if (Objects.nonNull(product.getSellerBadge())) {
      productAddProductToSolrEventModel.setSellerBadge(product.getSellerBadge().name());
    }
    return productAddProductToSolrEventModel;
  }

  public static SolrInputDocument toVendorSolrInputDocument(VendorProductSolr vendorProductSolr) {
    SolrInputDocument solrInputDocument = new SolrInputDocument();
    solrInputDocument.setField(VendorProductSolrFieldNames.STORE_ID, vendorProductSolr.getStoreId());
    solrInputDocument.setField(VendorProductSolrFieldNames.MARK_FOR_DELETE, vendorProductSolr.isMarkForDelete());
    solrInputDocument.setField(VendorProductSolrFieldNames.CREATED_DATE, vendorProductSolr.getCreatedDate());
    solrInputDocument.setField(VendorProductSolrFieldNames.CREATED_BY, vendorProductSolr.getCreatedBy());
    solrInputDocument.setField(VendorProductSolrFieldNames.UPDATED_DATE, vendorProductSolr.getUpdatedDate());
    solrInputDocument.setField(VendorProductSolrFieldNames.PRODUCT_CODE, vendorProductSolr.getProductCode());
    solrInputDocument.setField(VendorProductSolrFieldNames.PRODUCT_NAME, vendorProductSolr.getProductName());
    solrInputDocument.setField(VendorProductSolrFieldNames.POST_LIVE, vendorProductSolr.isPostLive());
    solrInputDocument.setField(VendorProductSolrFieldNames.CATEGORY_CODES, vendorProductSolr.getCategoryCodes());
    solrInputDocument.setField(VendorProductSolrFieldNames.CATEGORY_NAMES, vendorProductSolr.getCategoryNames());
    solrInputDocument.setField(VendorProductSolrFieldNames.BRAND, vendorProductSolr.getBrand());
    solrInputDocument.setField(VendorProductSolrFieldNames.BUSINESS_PARTNER_CODE,
        vendorProductSolr.getBusinessPartnerCode());
    solrInputDocument.setField(VendorProductSolrFieldNames.BUSINESS_PARTNER_NAME,
        vendorProductSolr.getBusinessPartnerName());
    solrInputDocument.setField(VendorProductSolrFieldNames.PRODUCT_CREATED_DATE,
        vendorProductSolr.getProductCreatedDate());
    solrInputDocument.setField(VendorProductSolrFieldNames.REJECTED_COUNT, vendorProductSolr.getRejectedCount());
    solrInputDocument.setField(VendorProductSolrFieldNames.STATE, vendorProductSolr.getState().toString());
    solrInputDocument.setField(VendorProductSolrFieldNames.VENDOR_CODE, vendorProductSolr.getVendorCode());
    solrInputDocument.setField(VendorProductSolrFieldNames.APPROVER_ASSIGNEE,
        StringUtils.isNotEmpty(vendorProductSolr.getProductApproverAssignee()) ?
            vendorProductSolr.getProductApproverAssignee() :
            SolrConstants.NOT_APPLICABLE);
    solrInputDocument.setField(VendorProductSolrFieldNames.ASSIGNED,
        Objects.nonNull(vendorProductSolr.getProductAssignedDate()));
    solrInputDocument.setField(VendorProductSolrFieldNames.ASSIGNED_DATE,
      vendorProductSolr.getProductAssignedDate());
    solrInputDocument.setField(VendorProductSolrFieldNames.APPROVED_DATE,
      vendorProductSolr.getProductApprovedDate());
    solrInputDocument.setField(VendorProductSolrFieldNames.BRAND_APPROVAL_STATUS,
      StringUtils.isNotEmpty(vendorProductSolr.getBrandApprovalStatus()) ?
        BrandApprovalStatus.valueOf(vendorProductSolr.getBrandApprovalStatus()).getValue() :
        null);
    solrInputDocument.setField(VendorProductSolrFieldNames.PRODUCT_PREDICTION_SCORE, vendorProductSolr.getProductPredictionScore());
    if (StringUtils.isEmpty(vendorProductSolr.getImageViolations())) {
      vendorProductSolr.setImageViolations(SolrConstants.NOT_APPLICABLE);
    }
    List<String> imageViolationsList = Arrays.asList(vendorProductSolr.getImageViolations().split(","));
    solrInputDocument.setField(VendorProductSolrFieldNames.IMAGE_VIOLATIONS, imageViolationsList);
    solrInputDocument.setField(VendorProductSolrFieldNames.QC_RETRY_COUNT, vendorProductSolr.getQcRetryCount());
    solrInputDocument.setField(VendorProductSolrFieldNames.PRODUCT_REVIEW_TYPE,
      getProductReviewType(vendorProductSolr.isEdited(), vendorProductSolr.isRevised()));
    solrInputDocument.setField(VendorProductSolrFieldNames.RESTRICTED_KEYWORDS_PRESENT,
      vendorProductSolr.isRestrictedKeywordsPresent());
    solrInputDocument.setField(VendorProductSolrFieldNames.REVIEW_TYPE,
      StringUtils.isNotEmpty(vendorProductSolr.getReviewType()) ?
        ReviewType.valueOf(vendorProductSolr.getReviewType()).getValue() :
        null);
    if (Objects.nonNull(vendorProductSolr.getSellerBadge())) {
      solrInputDocument.setField(VendorProductSolrFieldNames.SELLER_BADGE,
        SellerBadge.valueOf(vendorProductSolr.getSellerBadge()).getValue());
    }
    solrInputDocument.setField(VendorProductSolrFieldNames.SELLER_TYPE, vendorProductSolr.getSellerType());
    solrInputDocument.setField(VendorProductSolrFieldNames.APPEALED_PRODUCT,
      vendorProductSolr.isAppealedProduct());
    solrInputDocument.setField(VendorProductSolrFieldNames.PREDICTED_BRAND, vendorProductSolr.getPredictedBrand());
    solrInputDocument.setField(VendorProductSolrFieldNames.PRODUCT_CHANNEL, vendorProductSolr.getProductChannel());
    solrInputDocument.setField(VendorProductSolrFieldNames.DISTRIBUTION_MAPPING_STATUS,
        vendorProductSolr.getDistributionMappingStatus());
    solrInputDocument.setField(VendorProductSolrFieldNames.PRODUCT_CREATION_TYPE,
        vendorProductSolr.getProductCreationType());
    return solrInputDocument;
  }

  public static SolrQuery getSolrQueryForVendorList(String storeId, SummaryFilterDTO summaryFilterDTO,
      List<WorkflowState> states, Pageable pageable) {
    SolrQuery solrQuery = new SolrQuery();
    StringBuilder query = new StringBuilder();
    //fq
    solrQuery.addFilterQuery(
        new StringBuilder().append(VendorProductSolrFieldNames.STORE_ID).append(SolrConstants.COLON).append(storeId)
            .toString());
    solrQuery.setStart(pageable.getPageNumber() * pageable.getPageSize());
    solrQuery.setRows(pageable.getPageSize());
    solrQuery.set(CommonParams.TZ, SolrConstants.JKT_TIME_ZONE);

    //For mfd
    //solrQuery.setQuery(getQueryForMarkForDelete(false));
    query.append(getQueryForMarkForDelete(false));

    //For searchKeyword
    if (StringUtils.isNotBlank(summaryFilterDTO.getKeyword())) {
      query.append(SolrConstants.AND).append(SolrConstants.OPEN_BRACKET)
          .append(getSearchQueryBySearchKeyword(summaryFilterDTO.getKeyword())).append(SolrConstants.CLOSE_BRACKET);
    }
    //For states
    if (CollectionUtils.isNotEmpty(states)) {
      query.append(SolrConstants.AND).append(VendorProductSolrFieldNames.STATE).append(SolrConstants.COLON)
          .append(getQueryForStates(states));
    }

    //For timeFilter
    if (Objects.nonNull(summaryFilterDTO.getTimeFilterType())) {
      String timeFilterQuery = getQueryForProductCreatedDate(summaryFilterDTO.getTimeFilterType());
      if (!SolrConstants.ALL.equals(timeFilterQuery)) {
        query.append(SolrConstants.AND).append(VendorProductSolrFieldNames.PRODUCT_CREATED_DATE)
            .append(SolrConstants.COLON).append(timeFilterQuery);
      }
    }
    //For categoryCode
    if (StringUtils.isNotBlank(summaryFilterDTO.getCategoryCode())) {
      query.append(SolrConstants.AND).append(VendorProductSolrFieldNames.CATEGORY_CODES).append(SolrConstants.COLON)
          .append(summaryFilterDTO.getCategoryCode());
    }

    //For bpCode
    if (StringUtils.isNotBlank(summaryFilterDTO.getBusinessPartnerCode())) {
      if (SolrConstants.EXTERNAL.equalsIgnoreCase(summaryFilterDTO.getBusinessPartnerCode())) {
        query.append(SolrConstants.AND).append(SolrConstants.NOT)
            .append(VendorProductSolrFieldNames.BUSINESS_PARTNER_CODE).append(SolrConstants.COLON)
            .append(SolrConstants.DOUBLE_QUOTES).append(SolrConstants.INTERNAL).append(SolrConstants.DOUBLE_QUOTES);
      } else if (SolrConstants.TRUSTED_SELLER.equalsIgnoreCase(summaryFilterDTO.getBusinessPartnerCode())) {
        solrQuery.addFilterQuery(
            new StringBuilder().append(VendorProductSolrFieldNames.SELLER_TYPE).append(SolrConstants.COLON)
                .append(SellerType.TRUSTED_SELLER.getValue()).toString());
      } else {
        query.append(SolrConstants.AND).append(VendorProductSolrFieldNames.BUSINESS_PARTNER_CODE)
            .append(SolrConstants.COLON).append(SolrConstants.DOUBLE_QUOTES)
            .append(summaryFilterDTO.getBusinessPartnerCode()).append(SolrConstants.DOUBLE_QUOTES);
      }
    }

    //For assignee email ID
    if (StringUtils.isNotEmpty(summaryFilterDTO.getAssigneeEmailId())) {
      query.append(SolrConstants.AND).append(
        getQueryForAssignee(summaryFilterDTO.getAssigneeEmailId(),
          VendorProductSolrFieldNames.APPROVER_ASSIGNEE));
    }

    //For vendorCode
    //fq
    if (StringUtils.isNotEmpty(summaryFilterDTO.getVendorCode())) {
      solrQuery.addFilterQuery(
          new StringBuilder().append(VendorProductSolrFieldNames.VENDOR_CODE).append(SolrConstants.COLON)
              .append(summaryFilterDTO.getVendorCode()).toString());
    }

    //For post-live flag
    //fq
    if (Objects.nonNull(summaryFilterDTO.getPostLive())) {
      solrQuery.addFilterQuery(VendorProductSolrFieldNames.POST_LIVE + SolrConstants.COLON
        + summaryFilterDTO.getPostLive());
    } else {
      // include both post live and pre live products
      solrQuery.addFilterQuery(
        VendorProductSolrFieldNames.POST_LIVE + SolrConstants.COLON + SolrConstants.OPEN_BRACKET
          + String.join(SolrConstants.OR, String.valueOf(Boolean.TRUE),
          String.valueOf(Boolean.FALSE)) + SolrConstants.CLOSE_BRACKET);
    }
    //For product review tyoe
    String productReviewType = getProductReviewTypeForVendorList(summaryFilterDTO.getEdited(),
      summaryFilterDTO.getRevised());
    if (StringUtils.isNotEmpty(productReviewType)) {
      solrQuery.addFilterQuery(
        new StringBuilder().append(VendorProductSolrFieldNames.PRODUCT_REVIEW_TYPE)
          .append(SolrConstants.COLON).append(productReviewType).toString());
    }

    //For BrandPending
    //fq
    if (Objects.nonNull(summaryFilterDTO.getBrandPending()) && Boolean.TRUE.equals(
      summaryFilterDTO.getBrandPending())) {
      solrQuery.addFilterQuery(
        VendorProductSolrFieldNames.BRAND_APPROVAL_STATUS + SolrConstants.COLON
          + SolrConstants.OPEN_BRACKET + String.join(SolrConstants.OR,
          BrandApprovalStatus.DRAFT.getValue(), BrandApprovalStatus.REJECTED.getValue())
          + SolrConstants.CLOSE_BRACKET);
    }

    //For assignment
    if (Objects.nonNull(summaryFilterDTO.getAssignment())) {
      solrQuery.addFilterQuery(
        new StringBuilder().append(VendorProductSolrFieldNames.ASSIGNED).append(SolrConstants.COLON)
          .append(summaryFilterDTO.getAssignment()).toString());
    }

    //For faulty image type
    if (StringUtils.isNotEmpty(summaryFilterDTO.getFaultyImageType())) {
      query.append(SolrConstants.AND).append(getQueryForFaultyImageType(summaryFilterDTO.getFaultyImageType()));
    }

    //For productCreatedDate order
    if (ORDER_BY_DESC.equalsIgnoreCase(summaryFilterDTO.getSortOrderByCreatedDate())) {
      solrQuery.addSort(VendorProductSolrFieldNames.PRODUCT_CREATED_DATE, SolrQuery.ORDER.desc);
      solrQuery.addSort(VendorProductSolrFieldNames.PRODUCT_PREDICTION_SCORE, SolrQuery.ORDER.asc);
    } else {
      solrQuery.addSort(VendorProductSolrFieldNames.PRODUCT_CREATED_DATE, SolrQuery.ORDER.asc);
      solrQuery.addSort(VendorProductSolrFieldNames.PRODUCT_PREDICTION_SCORE, SolrQuery.ORDER.desc);
    }

    //For restricted keyword product
    if (Objects.nonNull(summaryFilterDTO.getRestrictedKeyword())) {
      solrQuery.addFilterQuery(
        new StringBuilder().append(VendorProductSolrFieldNames.RESTRICTED_KEYWORDS_PRESENT)
          .append(SolrConstants.COLON).append(summaryFilterDTO.getRestrictedKeyword()).toString());
    }

    //For review type
    if (Boolean.TRUE.equals(summaryFilterDTO.getContentPending()) || Boolean.TRUE.equals(
        summaryFilterDTO.getImagePending())) {
      solrQuery.addFilterQuery(
          new StringBuilder().append(VendorProductSolrFieldNames.REVIEW_TYPE).append(SolrConstants.COLON).append(
                  getReviewTypeFromRequest(summaryFilterDTO.getContentPending(), summaryFilterDTO.getImagePending()))
              .toString());
    }

    //For appealedProduct
    if (Objects.nonNull(summaryFilterDTO.getAppealedProduct())) {
      solrQuery.addFilterQuery(
          new StringBuilder().append(VendorProductSolrFieldNames.APPEALED_PRODUCT)
              .append(SolrConstants.COLON).append(summaryFilterDTO.getAppealedProduct())
              .toString());
    }

    //for product channel
    if (Boolean.TRUE.equals(summaryFilterDTO.getB2cActivated()) && Boolean.TRUE.equals(
        summaryFilterDTO.getB2bActivated())) {
      StringBuilder stringBuilder =
          new StringBuilder(VendorProductSolrFieldNames.PRODUCT_CHANNEL).append(SolrConstants.COLON)
              .append(SolrConstants.OPEN_BRACKET)
              .append(StringUtils.join(Arrays.asList(Constants.RETAIL, Constants.B2B), SolrConstants.AND))
              .append(SolrConstants.CLOSE_BRACKET);
      solrQuery.addFilterQuery(stringBuilder.toString());
    } else if (Boolean.TRUE.equals(summaryFilterDTO.getB2cActivated())) {
      StringBuilder stringBuilderInclusion =
          new StringBuilder(VendorProductSolrFieldNames.PRODUCT_CHANNEL).append(SolrConstants.COLON)
              .append(SolrConstants.DOUBLE_QUOTE).append(Constants.RETAIL).append(SolrConstants.DOUBLE_QUOTE);
      StringBuilder stringBuilderExclusion =
          new StringBuilder(SolrConstants.NOT).append(VendorProductSolrFieldNames.PRODUCT_CHANNEL)
              .append(SolrConstants.COLON).append(SolrConstants.DOUBLE_QUOTE).append(Constants.B2B)
              .append(SolrConstants.DOUBLE_QUOTE);
      solrQuery.addFilterQuery(stringBuilderInclusion.toString());
      solrQuery.addFilterQuery(stringBuilderExclusion.toString());
    } else if (Boolean.TRUE.equals(summaryFilterDTO.getB2bActivated())) {
      StringBuilder stringBuilderInclusion =
          new StringBuilder(VendorProductSolrFieldNames.PRODUCT_CHANNEL).append(SolrConstants.COLON)
              .append(SolrConstants.DOUBLE_QUOTE).append(Constants.B2B).append(SolrConstants.DOUBLE_QUOTE);
      StringBuilder stringBuilderExclusion =
          new StringBuilder(SolrConstants.NOT).append(VendorProductSolrFieldNames.PRODUCT_CHANNEL)
              .append(SolrConstants.COLON).append(SolrConstants.DOUBLE_QUOTE).append(Constants.RETAIL)
              .append(SolrConstants.DOUBLE_QUOTE);
      solrQuery.addFilterQuery(stringBuilderInclusion.toString());
      solrQuery.addFilterQuery(stringBuilderExclusion.toString());
    }

    solrQuery.setQuery(query.toString());
    return solrQuery;
  }

  public static SolrQuery getSolrQueryForIprProductList(String storeId,
    IPRProductListRequest iprProductListRequest, Pageable pageable) {
    SolrQuery solrQuery = new SolrQuery();
    StringBuilder query = new StringBuilder();
    //fq
    solrQuery.addFilterQuery(
      new StringBuilder().append(VendorProductSolrFieldNames.STORE_ID).append(SolrConstants.COLON)
        .append(storeId).toString());
    solrQuery.setStart(pageable.getPageNumber() * pageable.getPageSize());
    solrQuery.setRows(pageable.getPageSize());
    solrQuery.set(CommonParams.TZ, SolrConstants.JKT_TIME_ZONE);
    //For mfd
    //solrQuery.setQuery(getQueryForMarkForDelete(false));
    query.append(getQueryForMarkForDelete(false));
    //For searchKeyword
    if (StringUtils.isNotBlank(iprProductListRequest.getKeyword())) {
      query.append(SolrConstants.AND).append(SolrConstants.OPEN_BRACKET)
        .append(ClientUtils.escapeQueryChars(iprProductListRequest.getKeyword().trim()))
        .append(SolrConstants.CLOSE_BRACKET);
      solrQuery.set(SolrConstants.QF, IprProductSolrFieldNames.PRODUCT_NAME + StringUtils.SPACE
        + IprProductSolrFieldNames.PRODUCT_SKU);
      solrQuery.set(SolrConstants.MM, SolrConstants.MM_VALUE);
    }

    // for review status
    if (StringUtils.isNotBlank(iprProductListRequest.getState())) {
      query.append(SolrConstants.AND).append(IprProductSolrFieldNames.STATE)
        .append(SolrConstants.COLON)
        .append(ProductStateIPR.getValueFromName(iprProductListRequest.getState()));
    }

    //For timeFilter
    if (StringUtils.isNotBlank(iprProductListRequest.getTimeFilterWebType())) {
      TimeFilterType timeFilterType =
        TimeFilterType.getTimeFilterTypeByValue(iprProductListRequest.getTimeFilterWebType());
      String timeFilterQuery = getQueryForProductCreatedDate(timeFilterType);
      if (!SolrConstants.ALL.equals(timeFilterQuery)) {
        query.append(SolrConstants.AND).append(IprProductSolrFieldNames.PRODUCT_ADDED_DATE)
          .append(SolrConstants.COLON).append(timeFilterQuery);
      }
    }
    //For categoryCode
    if (StringUtils.isNotBlank(iprProductListRequest.getCategoryCode())) {
      query.append(SolrConstants.AND).append(IprProductSolrFieldNames.CATEGORY_CODE)
        .append(SolrConstants.COLON)
        .append(appendDoubleQuotes(iprProductListRequest.getCategoryCode()));
    }

    //For brandCode
    if (StringUtils.isNotBlank(iprProductListRequest.getBrandCode())) {
      query.append(SolrConstants.AND).append(IprProductSolrFieldNames.BRAND_CODE)
        .append(SolrConstants.COLON)
        .append(appendDoubleQuotes(iprProductListRequest.getBrandCode()));
    }

    // for business partner code
    if (StringUtils.isNotBlank(iprProductListRequest.getBusinessPartnerCode())) {
      query.append(SolrConstants.AND).append(IprProductSolrFieldNames.BUSINESS_PARTNER_CODE)
        .append(SolrConstants.COLON)
        .append(appendDoubleQuotes(iprProductListRequest.getBusinessPartnerCode()));
    }

    //For assignee email ID
    if (StringUtils.isNotEmpty(iprProductListRequest.getAssignedTo())) {
      query.append(SolrConstants.AND).append(
        getQueryForAssignee(iprProductListRequest.getAssignedTo(),
          IprProductSolrFieldNames.ASSIGNED_TO));
    }

    //For productCreatedDate order
    if (ORDER_BY_DESC.equalsIgnoreCase(iprProductListRequest.getSortOrder())) {
      solrQuery.addSort(IprProductSolrFieldNames.PRODUCT_ADDED_DATE, SolrQuery.ORDER.desc);
    } else {
      solrQuery.addSort(IprProductSolrFieldNames.PRODUCT_ADDED_DATE, SolrQuery.ORDER.asc);
    }

    generateQueryForAssignedUnAssignedFilter(query, iprProductListRequest);

    solrQuery.setQuery(query.toString());
    return solrQuery;
  }

  private static void generateQueryForAssignedUnAssignedFilter(StringBuilder query,
    IPRProductListRequest iprProductListRequest) {
    //For assigned
    if (Boolean.TRUE.equals(iprProductListRequest.getAssigned())) {
      query.append(SolrConstants.AND).append(IprProductSolrFieldNames.ASSIGNED_TO)
        .append(SolrConstants.COLON).append(SolrConstants.ASSIGNED_PRESENT);
    }

    //For unassigned
    if (Boolean.FALSE.equals(iprProductListRequest.getAssigned())) {
      query.append(SolrConstants.AND).append(SolrConstants.IPR_PRODUCT_NOT_ASSIGNED)
        .append(SolrConstants.COLON).append(SolrConstants.LIKE_QUERY);
    }
  }

  private static String getProductReviewTypeForVendorList(Boolean edited, Boolean revised) {
    if (Boolean.TRUE.equals(revised) && Boolean.FALSE.equals(edited)) {
      return ProductReviewType.REVISED.getValue();
    } else if (Boolean.TRUE.equals(edited) && Boolean.FALSE.equals(revised)) {
      return ProductReviewType.EDITED.getValue();
    } else if (Boolean.FALSE.equals(edited) && Boolean.FALSE.equals(revised)) {
      return ProductReviewType.NEWLY_ADDED.getValue();
    } else if (Boolean.TRUE.equals(edited) && Boolean.TRUE.equals(revised)) {
      return SolrConstants.OPEN_BRACKET + String.join(SolrConstants.OR,
        ImmutableSet.of(ProductReviewType.REVISED.getValue(),
          ProductReviewType.EDITED.getValue())) + SolrConstants.CLOSE_BRACKET;
    } else if (Objects.isNull(edited) && Boolean.FALSE.equals(revised)) {
      //include both newly added and edited if both are selected from multi select(revised False)
      return SolrConstants.OPEN_BRACKET + String.join(SolrConstants.OR,
        ImmutableSet.of(ProductReviewType.NEWLY_ADDED.getValue(),
          ProductReviewType.EDITED.getValue())) + SolrConstants.CLOSE_BRACKET;
    } else if (Objects.isNull(revised) && Boolean.FALSE.equals(edited)) {
      return SolrConstants.OPEN_BRACKET + String.join(SolrConstants.OR,
        ImmutableSet.of(ProductReviewType.NEWLY_ADDED.getValue(),
          ProductReviewType.REVISED.getValue())) + SolrConstants.CLOSE_BRACKET;
    } else if ((Objects.isNull(edited) && Objects.isNull(revised))) {
      // if all the dropDowns are selected or none is selected (newly added + revised + edited)
      return SolrConstants.OPEN_BRACKET + String.join(SolrConstants.OR,
        ImmutableSet.of(ProductReviewType.NEWLY_ADDED.getValue(),
          ProductReviewType.REVISED.getValue(), ProductReviewType.EDITED.getValue()))
        + SolrConstants.CLOSE_BRACKET;
    }
    return StringUtils.EMPTY;
  }

  private static String getProductReviewType(boolean edited, boolean revised) {
    if (revised) {
      return ProductReviewType.REVISED.getValue();
    } else if (edited) {
      return ProductReviewType.EDITED.getValue();
    } else {
      return ProductReviewType.NEWLY_ADDED.getValue();
    }
  }

  private static String getProductReviewTypeForFilterCount(Boolean edited, Boolean revised) {
    if (Boolean.TRUE.equals(revised) && Boolean.FALSE.equals(edited)) {
      return ProductReviewType.REVISED.getValue();
    } else if (Boolean.TRUE.equals(edited) && Boolean.FALSE.equals(revised)) {
      return ProductReviewType.EDITED.getValue();
    } else if(Boolean.FALSE.equals(edited) && Boolean.FALSE.equals(revised)){
      return ProductReviewType.NEWLY_ADDED.getValue();
    }
    else if ((Objects.isNull(edited) && Objects.isNull(revised))) {
      return SolrConstants.OPEN_BRACKET + String.join(SolrConstants.OR,
        ImmutableSet.of(ProductReviewType.NEWLY_ADDED.getValue(),
          ProductReviewType.REVISED.getValue(), ProductReviewType.EDITED.getValue()))
        + SolrConstants.CLOSE_BRACKET;
    }
    return StringUtils.EMPTY;
  }

  private static String getReviewTypeFromRequest(Boolean contentPending, Boolean imagePending) {
    if (Boolean.TRUE.equals(contentPending) && !Boolean.TRUE.equals(imagePending)) {
      return String.valueOf(ReviewType.CONTENT.getValue());
    } else if (!Boolean.TRUE.equals(contentPending) && Boolean.TRUE.equals(imagePending)) {
      return String.valueOf(ReviewType.IMAGE.getValue());
    } else {
      return String.valueOf(ReviewType.CONTENT_AND_IMAGE.getValue());
    }
  }

  public static SolrQuery getSolrQueryForProductList(
      DistributionTaskMultipleFilterDTO distributionTaskMultipleFilterDTO, Pageable pageable) {
    SolrQuery solrQuery = new SolrQuery();
    StringBuilder query = new StringBuilder();

    //fq storeId
    solrQuery.addFilterQuery(
        new StringBuilder().append(VendorProductSolrFieldNames.STORE_ID).append(SolrConstants.COLON)
            .append(distributionTaskMultipleFilterDTO.getStoreId()).append(SolrConstants.AND)
            .append(getQueryForMarkForDelete(false)).toString());
    solrQuery.setStart(pageable.getPageNumber() * pageable.getPageSize());
    solrQuery.setRows(pageable.getPageSize());
    solrQuery.set(CommonParams.TZ, SolrConstants.JKT_TIME_ZONE);

    //For searchKeyword -> ProductName
    if (StringUtils.isNotBlank(distributionTaskMultipleFilterDTO.getProductName())) {
      query.append(SolrConstants.OPEN_BRACKET)
          .append(getSearchQueryByProductNameSearchKeyword(distributionTaskMultipleFilterDTO.getProductName()))
          .append(SolrConstants.CLOSE_BRACKET);
    }

    //For categoryCode
    if (StringUtils.isNotBlank(distributionTaskMultipleFilterDTO.getCategoryCode())) {
      if (StringUtils.isNotBlank(query)) {
        query.append(SolrConstants.AND);
      }
      query.append(VendorProductSolrFieldNames.CATEGORY_CODES).append(SolrConstants.COLON)
          .append(SolrConstants.OPEN_BRACKET).append(SolrConstants.DOUBLE_QUOTES).append(
              ClientUtils.escapeQueryChars(StringUtils.trimToEmpty(distributionTaskMultipleFilterDTO.getCategoryCode())))
          .append(SolrConstants.DOUBLE_QUOTES).append(SolrConstants.CLOSE_BRACKET);
    }

    //For bpCode
    if (StringUtils.isNotBlank(distributionTaskMultipleFilterDTO.getBusinessPartnerCode())) {
      if (StringUtils.isNotBlank(query)) {
        query.append(SolrConstants.AND);
      }
      if (SolrConstants.EXTERNAL.equalsIgnoreCase(distributionTaskMultipleFilterDTO.getBusinessPartnerCode())) {
        query.append(SolrConstants.NOT).append(VendorProductSolrFieldNames.BUSINESS_PARTNER_CODE).append(SolrConstants.COLON)
            .append(SolrConstants.DOUBLE_QUOTES).append(SolrConstants.INTERNAL).append(SolrConstants.DOUBLE_QUOTES);
      } else{
        query.append(VendorProductSolrFieldNames.BUSINESS_PARTNER_CODE).append(SolrConstants.COLON)
            .append(distributionTaskMultipleFilterDTO.getBusinessPartnerCode());
      }
    }

    //For status list
    if (CollectionUtils.isNotEmpty(distributionTaskMultipleFilterDTO.getStatusList())) {
      if (StringUtils.isNotBlank(query)) {
        query.append(SolrConstants.AND);
      }
      query.append(VendorProductSolrFieldNames.STATE).append(SolrConstants.COLON)
          .append(getQueryForStates(distributionTaskMultipleFilterDTO.getStatusList()));
    }

    //For timeFilter
    if (Objects.nonNull(distributionTaskMultipleFilterDTO.getTimeFilterType())) {
      if (StringUtils.isNotBlank(query)) {
        query.append(SolrConstants.AND);
      }
      String timeFilterQuery = getQueryForProductCreatedDate(distributionTaskMultipleFilterDTO.getTimeFilterType());
      if (!SolrConstants.ALL.equals(timeFilterQuery)) {
        query.append(VendorProductSolrFieldNames.PRODUCT_CREATED_DATE).append(SolrConstants.COLON)
            .append(timeFilterQuery);
      }
    }

    //For vendor code list
    if (CollectionUtils.isNotEmpty(distributionTaskMultipleFilterDTO.getVendorCodes())) {
      if (StringUtils.isNotBlank(query)) {
        query.append(SolrConstants.AND);
      }
      query.append(VendorProductSolrFieldNames.VENDOR_CODE).append(SolrConstants.COLON)
          .append(getQueryForVendors(distributionTaskMultipleFilterDTO.getVendorCodes()));
    }

    //For rejected list
    if (CollectionUtils.isNotEmpty(distributionTaskMultipleFilterDTO.getRejectedList())) {
      if (StringUtils.isNotBlank(query)) {
        query.append(SolrConstants.AND);
      }
      query.append(VendorProductSolrFieldNames.REJECTED_COUNT).append(SolrConstants.COLON)
          .append(getQueryForRejectedList(distributionTaskMultipleFilterDTO.getRejectedList()));
    }

    //Query should not be empty so a dummy query is added if query is empty
    if (StringUtils.isBlank(query)) {
      query.append(getQueryForMarkForDelete(false));
    }

    //For productCreatedDate order
    if (ORDER_BY_DESC.equalsIgnoreCase(distributionTaskMultipleFilterDTO.getSortOrderByCreatedDate())) {
      solrQuery.addSort(VendorProductSolrFieldNames.PRODUCT_CREATED_DATE, SolrQuery.ORDER.desc);
    } else {
      solrQuery.addSort(VendorProductSolrFieldNames.PRODUCT_CREATED_DATE, SolrQuery.ORDER.asc);
    }

    solrQuery.setQuery(query.toString());
    return solrQuery;
  }

  public static SolrQuery getSolrQueryForFilterProductSummary(String storeId, ProductListRequest productListRequest,
      List<WorkflowState> states, Pageable pageable) {
    SolrQuery solrQuery = new SolrQuery();
    StringBuilder query = new StringBuilder();
    //fq
    solrQuery.addFilterQuery(
        new StringBuilder().append(VendorProductSolrFieldNames.STORE_ID).append(SolrConstants.COLON).append(storeId)
            .toString());
    solrQuery.setStart(pageable.getPageNumber() * pageable.getPageSize());
    solrQuery.setRows(pageable.getPageSize());
    solrQuery.set(CommonParams.TZ, SolrConstants.JKT_TIME_ZONE);

    //For mfd
    query.append(getQueryForMarkForDelete(false));

    //searchKey
    if (StringUtils.isNotBlank(productListRequest.getProductName())) {
      query.append(SolrConstants.AND).append(SolrConstants.OPEN_BRACKET)
          .append(getSearchQueryBySearchKeyword(productListRequest.getProductName()))
          .append(SolrConstants.CLOSE_BRACKET);
    }

    //For states
    if (CollectionUtils.isNotEmpty(states)) {
      query.append(SolrConstants.AND).append(VendorProductSolrFieldNames.STATE).append(SolrConstants.COLON)
          .append(getQueryForStates(states));
    }

    //businessPartnerCode
    if (StringUtils.isNotBlank(productListRequest.getBusinessPartnerCode())) {
      if (SolrConstants.EXTERNAL.equalsIgnoreCase(productListRequest.getBusinessPartnerCode())) {
        query.append(SolrConstants.AND).append(SolrConstants.NOT)
            .append(VendorProductSolrFieldNames.BUSINESS_PARTNER_CODE).append(SolrConstants.COLON)
            .append(SolrConstants.DOUBLE_QUOTES).append(SolrConstants.INTERNAL).append(SolrConstants.DOUBLE_QUOTES);
      }
      else{
        query.append(SolrConstants.AND).append(VendorProductSolrFieldNames.BUSINESS_PARTNER_CODE)
            .append(SolrConstants.COLON).append(SolrConstants.DOUBLE_QUOTES)
            .append(productListRequest.getBusinessPartnerCode()).append(SolrConstants.DOUBLE_QUOTES);
      }
    }

    //categoryCode
    if (StringUtils.isNotBlank(productListRequest.getCategoryCode())) {
      query.append(SolrConstants.AND).append(VendorProductSolrFieldNames.CATEGORY_CODES).append(SolrConstants.COLON)
          .append(SolrConstants.DOUBLE_QUOTE).append(productListRequest.getCategoryCode())
          .append(SolrConstants.DOUBLE_QUOTE);
    }

    //timeFilterType
    if (StringUtils.isNotBlank(productListRequest.getTimeFilterType())) {
      String timeFilterQuery = getQueryForProductCreatedDate(
          TimeFilterType.getTimeFilterTypeByValue(productListRequest.getTimeFilterType()));
      if (!SolrConstants.ALL.equals(timeFilterQuery)) {
        query.append(SolrConstants.AND).append(VendorProductSolrFieldNames.PRODUCT_CREATED_DATE)
            .append(SolrConstants.COLON).append(timeFilterQuery);
      }
    }

    //sort
    solrQuery.addSort(VendorProductSolrFieldNames.PRODUCT_CREATED_DATE, SolrQuery.ORDER.desc);
    solrQuery.addSort(VendorProductSolrFieldNames.PRODUCT_PREDICTION_SCORE, SolrQuery.ORDER.asc);

    solrQuery.setQuery(query.toString());
    return solrQuery;
  }

  private static String getQueryForMarkForDelete(boolean markForDelete) {
    return new StringBuilder().append(VendorProductSolrFieldNames.MARK_FOR_DELETE).append(SolrConstants.COLON)
        .append(markForDelete).toString();
  }

  private static String getQueryForFaultyImageType(String faultyImageType) {
    if (ProductLabels.GOOD.getDescription().equalsIgnoreCase(faultyImageType)) {
      return new StringBuilder().append(VendorProductSolrFieldNames.IMAGE_VIOLATIONS).append(SolrConstants.COLON)
          .append(SolrConstants.OPEN_BRACKET).append(SolrConstants.NOT_APPLICABLE).append(SolrConstants.CLOSE_BRACKET)
          .toString();
    } else {
      return new StringBuilder().append(VendorProductSolrFieldNames.IMAGE_VIOLATIONS).append(SolrConstants.COLON)
          .append(SolrConstants.OPEN_BRACKET).append(SolrConstants.DOUBLE_QUOTES).append(faultyImageType)
          .append(SolrConstants.DOUBLE_QUOTES).append(SolrConstants.CLOSE_BRACKET).toString();
    }
  }

  public static void setAssignedFilter(Boolean assignment, StringBuilder query) {
    if (Objects.nonNull(assignment)) {
      String unAssigned =
          new StringBuilder(SolrConstants.OPEN_BRACKET).append(VendorProductSolrFieldNames.APPROVER_ASSIGNEE)
              .append(SolrConstants.COLON).append(appendDoubleQuotes(SolrConstants.NOT_APPLICABLE))
              .append(SolrConstants.CLOSE_BRACKET).toString();
      if (Boolean.FALSE.equals(assignment)) {
        query.append(SolrConstants.AND).append(unAssigned);
      } else if (Boolean.TRUE.equals(assignment)) {
        query.append(SolrConstants.AND).append(SolrConstants.NOT_IN).append(unAssigned);
      }
    }
  }

  public static String appendDoubleQuotes(String string) {
    return new StringBuilder(SolrConstants.DOUBLE_QUOTE).append(string).append(SolrConstants.DOUBLE_QUOTE).toString();
  }

  private static String getQueryForAssignee(String assigneeEmailId, String fieldName) {
    return new StringBuilder().append(SolrConstants.OPEN_BRACKET)
      .append(fieldName).append(SolrConstants.COLON)
      .append(appendDoubleQuotes(assigneeEmailId)).append(SolrConstants.CLOSE_BRACKET).toString();
  }

  public static String getQueryForProductCreatedDate(TimeFilterType timeFilterType) {
    if (TimeFilterType.TODAY.equals(timeFilterType)) {
      return SolrConstants.TODAY_FACET_INTERVAL.replace(SolrConstants.COMMA, StringUtils.SPACE + SolrConstants.TO);
    } else if (TimeFilterType.YESTERDAY.equals(timeFilterType)) {
      return SolrConstants.YESTERDAY_FACET_INTERVAL.replace(SolrConstants.COMMA, StringUtils.SPACE + SolrConstants.TO);
    } else if (TimeFilterType.TWO_DAYS_AGO.equals(timeFilterType)) {
      return SolrConstants.TWO_DAYS_AGO_FACET_INTERVAL.replace(SolrConstants.COMMA,
          StringUtils.SPACE + SolrConstants.TO);
    } else if (TimeFilterType.THREE_TO_FIVE_DAYS_AGO.equals(timeFilterType)) {
      return SolrConstants.THREE_TO_FIVE_DAYS_AGO_FACET_INTERVAL.replace(SolrConstants.COMMA,
          StringUtils.SPACE + SolrConstants.TO);
    } else if (TimeFilterType.FIVE_DAYS_AGO.equals(timeFilterType)) {
      return SolrConstants.FIVE_DAYS_AGO_FACET_INTERVAL.replace(SolrConstants.COMMA,
          StringUtils.SPACE + SolrConstants.TO);
    } else if (TimeFilterType.TWO_TO_THREE_DAYS_OLD.equals(timeFilterType)) {
      return SolrConstants.TWO_DAYS_AGO_FACET_INTERVAL.replace(SolrConstants.COMMA,
          StringUtils.SPACE + SolrConstants.TO);
    } else if (TimeFilterType.THREE_DAYS_AGO.equals(timeFilterType)) {
      return SolrConstants.THREE_DAYS_AGO_FACET_INTERVAL.replace(SolrConstants.COMMA,
          StringUtils.SPACE + SolrConstants.TO);
    } else {
      return SolrConstants.ALL;
    }
  }

  private static String getQueryForStates(List<WorkflowState> states) {
    StringBuilder statesQuery = new StringBuilder().append(SolrConstants.OPEN_BRACKET);
    for (WorkflowState workflowState : states) {
      statesQuery.append(workflowState.toString()).append(StringUtils.SPACE);
    }
    return statesQuery.append(SolrConstants.CLOSE_BRACKET).toString();
  }

  private static String getQueryForVendors(List<String> vendorCodes) {
    StringBuilder vendorQuery = new StringBuilder().append(SolrConstants.OPEN_BRACKET);
    for (String vendorCode : vendorCodes) {
      vendorQuery.append(vendorCode).append(StringUtils.SPACE);
    }
    return vendorQuery.append(SolrConstants.CLOSE_BRACKET).toString();
  }

  private static String getQueryForRejectedList(List<Integer> rejectedList) {
    StringBuilder rejectedQuery = new StringBuilder().append(SolrConstants.OPEN_BRACKET);
    for (Integer rejectedCount : rejectedList) {
      if (rejectedCount == SolrConstants.REJECTED_COUNT_GREATER_THAN_2) {
        rejectedQuery.append(SolrConstants.OPEN_SQUARE_BRACKET).append(rejectedCount).append(StringUtils.SPACE)
            .append(SolrConstants.TO).append(StringUtils.SPACE).append(SolrConstants.LIKE_QUERY)
            .append(SolrConstants.CLOSE_SQUARE_BRACKET);
      } else {
        rejectedQuery.append(rejectedCount).append(StringUtils.SPACE);
      }
    }
    return rejectedQuery.append(SolrConstants.CLOSE_BRACKET).toString();
  }

  private static StringBuilder getSearchQueryBySearchKeyword(String searchKeyword) {
    String keywordWithoutSpecialChars =
        getSearchKeywordWithoutSpecialCharacters(new StringBuilder(StringUtils.trimToEmpty(searchKeyword)));
    StringBuilder query = new StringBuilder();
    if (StringUtils.isNotEmpty(searchKeyword)) {
      getKeywordQuery(StringUtils.trimToEmpty(searchKeyword), searchKeyword, keywordWithoutSpecialChars, query);
    }
    return query;
  }

  private static StringBuilder getSearchQueryByProductNameSearchKeyword(String searchKeyword) {
    String keywordWithoutSpecialChars =
        getSearchKeywordWithoutSpecialCharacters(new StringBuilder(StringUtils.trimToEmpty(searchKeyword)));
    StringBuilder query = new StringBuilder();
    if (StringUtils.isNotEmpty(searchKeyword)) {
      getKeywordQueryProductName(StringUtils.trimToEmpty(searchKeyword), keywordWithoutSpecialChars, query);
    }
    return query;
  }

  private static String getSearchKeywordWithoutSpecialCharacters(StringBuilder searchKeyword) {
    Matcher matcher = PATTERN_FOR_SPECIAL_CHARACTERS.matcher(searchKeyword.toString());
    StringBuilder keyword = new StringBuilder(getSearchKeyword(matcher.replaceAll(StringUtils.SPACE)));
    return keyword.append(SolrConstants.LIKE_QUERY).toString();
  }

  private static String getSearchKeyword(String searchKeyword) {
    return String.join(SolrConstants.AND_WITH_FUZZY_SEARCH,
        StringUtils.trimToEmpty(searchKeyword).split(REGEX_FOR_SPACE));
  }

  private static void getKeywordQuery(String keyword, String searchKeyword, String keywordWithoutSpecialChars,
      StringBuilder query) {
    query.append(SolrConstants.OPEN_BRACKET).append(VendorProductSolrFieldNames.PRODUCT_NAME)
        .append(SolrConstants.COLON).append(SolrConstants.OPEN_BRACKET).append(keywordWithoutSpecialChars)
        .append(SolrConstants.CLOSE_BRACKET).append(StringUtils.SPACE).append(SolrConstants.OR)
        .append(StringUtils.SPACE).append(VendorProductSolrFieldNames.PRODUCT_CODE).append(SolrConstants.COLON)
        .append(SolrConstants.OPEN_BRACKET).append(SolrConstants.DOUBLE_QUOTES)
        .append(ClientUtils.escapeQueryChars(keyword)).append(SolrConstants.DOUBLE_QUOTES)
        .append(SolrConstants.CLOSE_BRACKET).append(StringUtils.SPACE).append(SolrConstants.OR)
        .append(StringUtils.SPACE).append(VendorProductSolrFieldNames.CREATED_BY).append(SolrConstants.COLON)
        .append(SolrConstants.OPEN_BRACKET).append(ClientUtils.escapeQueryChars(searchKeyword))
        .append(SolrConstants.LIKE_QUERY).append(SolrConstants.CLOSE_BRACKET).append(SolrConstants.CLOSE_BRACKET);
  }

  private static void getKeywordQueryProductName(String keyword, String keywordWithoutSpecialChars,
      StringBuilder query) {
    query.append(SolrConstants.OPEN_BRACKET).append(VendorProductSolrFieldNames.PRODUCT_NAME)
        .append(SolrConstants.COLON).append(SolrConstants.OPEN_BRACKET).append(keywordWithoutSpecialChars)
        .append(SolrConstants.CLOSE_BRACKET).append(StringUtils.SPACE).append(SolrConstants.OR)
        .append(StringUtils.SPACE).append(VendorProductSolrFieldNames.PRODUCT_CODE).append(SolrConstants.COLON)
        .append(SolrConstants.OPEN_BRACKET).append(SolrConstants.DOUBLE_QUOTES)
        .append(ClientUtils.escapeQueryChars(keyword)).append(SolrConstants.DOUBLE_QUOTES)
        .append(SolrConstants.CLOSE_BRACKET).append(SolrConstants.CLOSE_BRACKET);
  }

  public static IPRProductSolr getSolrIprProductCollection(SolrDocument solrDocument) {
    IPRProductSolr iprProductSolr = new IPRProductSolr();
    if (Objects.nonNull(solrDocument.getFieldValue(IprProductSolrFieldNames.STORE_ID))) {
      iprProductSolr.setStoreId(
        String.valueOf(solrDocument.getFieldValue(IprProductSolrFieldNames.STORE_ID)));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(IprProductSolrFieldNames.MARK_FOR_DELETE))) {
      iprProductSolr.setMarkForDelete(
        (boolean) solrDocument.getFieldValue(IprProductSolrFieldNames.MARK_FOR_DELETE));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(IprProductSolrFieldNames.CREATED_DATE))) {
      iprProductSolr.setCreatedDate(
        (Date) solrDocument.getFieldValue(IprProductSolrFieldNames.CREATED_DATE));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(IprProductSolrFieldNames.CREATED_BY))) {
      iprProductSolr.setCreatedBy(
        String.valueOf(solrDocument.getFieldValue(IprProductSolrFieldNames.CREATED_BY)));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(IprProductSolrFieldNames.UPDATED_DATE))) {
      iprProductSolr.setUpdatedDate(
        (Date) solrDocument.getFieldValue(IprProductSolrFieldNames.UPDATED_DATE));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(IprProductSolrFieldNames.PRODUCT_ADDED_DATE))) {
      iprProductSolr.setProductAddedDate(
        (Date) solrDocument.getFieldValue(IprProductSolrFieldNames.PRODUCT_ADDED_DATE));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(IprProductSolrFieldNames.PRODUCT_CODE))) {
      iprProductSolr.setProductCode(
        String.valueOf(solrDocument.getFieldValue(IprProductSolrFieldNames.PRODUCT_CODE)));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(IprProductSolrFieldNames.PRODUCT_NAME))) {
      iprProductSolr.setProductName(
        String.valueOf(solrDocument.getFieldValue(IprProductSolrFieldNames.PRODUCT_NAME)));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(IprProductSolrFieldNames.PRODUCT_SKU))) {
      iprProductSolr.setProductSku(
        String.valueOf(solrDocument.getFieldValue(IprProductSolrFieldNames.PRODUCT_SKU)));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(IprProductSolrFieldNames.CATEGORY_CODE))) {
      iprProductSolr.setCategoryCode(
        String.valueOf(solrDocument.getFieldValue(IprProductSolrFieldNames.CATEGORY_CODE)));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(IprProductSolrFieldNames.CATEGORY_NAME))) {
      iprProductSolr.setCategoryName(
        String.valueOf(solrDocument.getFieldValue(IprProductSolrFieldNames.CATEGORY_NAME)));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(IprProductSolrFieldNames.BRAND_CODE))) {
      iprProductSolr.setBrandCode(
        String.valueOf(solrDocument.getFieldValue(IprProductSolrFieldNames.BRAND_CODE)));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(IprProductSolrFieldNames.BRAND_NAME))) {
      iprProductSolr.setBrandName(
        String.valueOf(solrDocument.getFieldValue(IprProductSolrFieldNames.BRAND_NAME)));
    }
    if (Objects.nonNull(
      solrDocument.getFieldValue(IprProductSolrFieldNames.BUSINESS_PARTNER_CODE))) {
      iprProductSolr.setBusinessPartnerCode(
        String.valueOf(solrDocument.getFieldValue(IprProductSolrFieldNames.BUSINESS_PARTNER_CODE)));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(IprProductSolrFieldNames.STATE))) {
      iprProductSolr.setState(ProductStateIPR.fromValue(
        (int) solrDocument.getFieldValue(IprProductSolrFieldNames.STATE)));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(IprProductSolrFieldNames.ASSIGNED_TO))) {
      iprProductSolr.setAssignedTo(
        String.valueOf(solrDocument.getFieldValue(IprProductSolrFieldNames.ASSIGNED_TO)));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(IprProductSolrFieldNames.ASSIGNED_DATE))) {
      iprProductSolr.setAssignedDate(
        (Date) solrDocument.getFieldValue(IprProductSolrFieldNames.ASSIGNED_DATE));
    }
    if(Objects.nonNull(solrDocument.getFieldValue(IprProductSolrFieldNames.SOURCE))) {
      iprProductSolr.setSource(
          String.valueOf(solrDocument.getFieldValue(IprProductSolrFieldNames.SOURCE)));
    }
    return iprProductSolr;
  }

  public static VendorProductSolr getSolrProductCollectionDTO(SolrDocument solrDocument) {
    VendorProductSolr vendorProductSolr = new VendorProductSolr();
    if (Objects.nonNull(solrDocument.getFieldValue(VendorProductSolrFieldNames.STORE_ID))) {
      vendorProductSolr.setStoreId(String.valueOf(solrDocument.getFieldValue(VendorProductSolrFieldNames.STORE_ID)));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(VendorProductSolrFieldNames.MARK_FOR_DELETE))) {
      vendorProductSolr.setMarkForDelete(
          (boolean) solrDocument.getFieldValue(VendorProductSolrFieldNames.MARK_FOR_DELETE));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(VendorProductSolrFieldNames.CREATED_DATE))) {
      vendorProductSolr.setCreatedDate((Date) solrDocument.getFieldValue(VendorProductSolrFieldNames.CREATED_DATE));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(VendorProductSolrFieldNames.CREATED_BY))) {
      vendorProductSolr.setCreatedBy(
          String.valueOf(solrDocument.getFieldValue(VendorProductSolrFieldNames.CREATED_BY)));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(VendorProductSolrFieldNames.UPDATED_DATE))) {
      vendorProductSolr.setUpdatedDate((Date) solrDocument.getFieldValue(VendorProductSolrFieldNames.UPDATED_DATE));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(VendorProductSolrFieldNames.PRODUCT_CODE))) {
      vendorProductSolr.setProductCode(
          String.valueOf(solrDocument.getFieldValue(VendorProductSolrFieldNames.PRODUCT_CODE)));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(VendorProductSolrFieldNames.PRODUCT_NAME))) {
      vendorProductSolr.setProductName(
          String.valueOf(solrDocument.getFieldValue(VendorProductSolrFieldNames.PRODUCT_NAME)));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(VendorProductSolrFieldNames.POST_LIVE))) {
      vendorProductSolr.setPostLive((boolean) solrDocument.getFieldValue(VendorProductSolrFieldNames.POST_LIVE));
    }
    if (CollectionUtils.isNotEmpty(
        (List<String>) solrDocument.getFieldValue(VendorProductSolrFieldNames.CATEGORY_CODES))) {
      vendorProductSolr.setCategoryCodes(
          (((List<String>) solrDocument.getFieldValue(VendorProductSolrFieldNames.CATEGORY_CODES))));
    }
    if (CollectionUtils.isNotEmpty(
        (List<String>) solrDocument.getFieldValue(VendorProductSolrFieldNames.CATEGORY_NAMES))) {
      vendorProductSolr.setCategoryNames(
          (((List<String>) solrDocument.getFieldValue(VendorProductSolrFieldNames.CATEGORY_NAMES))));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(VendorProductSolrFieldNames.BRAND))) {
      vendorProductSolr.setBrand(String.valueOf(solrDocument.getFieldValue(VendorProductSolrFieldNames.BRAND)));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(VendorProductSolrFieldNames.BUSINESS_PARTNER_CODE))) {
      vendorProductSolr.setBusinessPartnerCode(
          String.valueOf(solrDocument.getFieldValue(VendorProductSolrFieldNames.BUSINESS_PARTNER_CODE)));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(VendorProductSolrFieldNames.BUSINESS_PARTNER_NAME))) {
      vendorProductSolr.setBusinessPartnerName(
          String.valueOf(solrDocument.getFieldValue(VendorProductSolrFieldNames.BUSINESS_PARTNER_NAME)));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(VendorProductSolrFieldNames.PRODUCT_CREATED_DATE))) {
      vendorProductSolr.setProductCreatedDate(
          (Date) solrDocument.getFieldValue(VendorProductSolrFieldNames.PRODUCT_CREATED_DATE));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(VendorProductSolrFieldNames.REJECTED_COUNT))) {
      vendorProductSolr.setRejectedCount((int) solrDocument.getFieldValue(VendorProductSolrFieldNames.REJECTED_COUNT));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(VendorProductSolrFieldNames.STATE))) {
      vendorProductSolr.setState(
          WorkflowState.valueOf(String.valueOf(solrDocument.getFieldValue(VendorProductSolrFieldNames.STATE))));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(VendorProductSolrFieldNames.VENDOR_CODE))) {
      vendorProductSolr.setVendorCode(
          String.valueOf(solrDocument.getFieldValue(VendorProductSolrFieldNames.VENDOR_CODE)));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(VendorProductSolrFieldNames.APPROVER_ASSIGNEE))) {
      vendorProductSolr.setProductApproverAssignee(
          String.valueOf(solrDocument.getFieldValue(VendorProductSolrFieldNames.APPROVER_ASSIGNEE)));
    }
    if (Objects
        .nonNull(solrDocument.getFieldValue(VendorProductSolrFieldNames.ASSIGNED_DATE))) {
      vendorProductSolr.setProductAssignedDate(
          (Date) solrDocument.getFieldValue(VendorProductSolrFieldNames.ASSIGNED_DATE));
    }
    if (Objects
        .nonNull(solrDocument.getFieldValue(VendorProductSolrFieldNames.APPROVED_DATE))) {
      vendorProductSolr.setProductApprovedDate(
          (Date) solrDocument.getFieldValue(VendorProductSolrFieldNames.APPROVED_DATE));
    }
    if (Objects.nonNull(
      solrDocument.getFieldValue(VendorProductSolrFieldNames.BRAND_APPROVAL_STATUS))) {
      vendorProductSolr.setBrandApprovalStatus(BrandApprovalStatus.getBrandApprovalStatusByValue(
        String.valueOf(
          solrDocument.getFieldValue(VendorProductSolrFieldNames.BRAND_APPROVAL_STATUS))));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(VendorProductSolrFieldNames.PRODUCT_PREDICTION_SCORE))) {
      vendorProductSolr.setProductPredictionScore(
          (int) solrDocument.getFieldValue(VendorProductSolrFieldNames.PRODUCT_PREDICTION_SCORE));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(VendorProductSolrFieldNames.IMAGE_VIOLATIONS))) {
      vendorProductSolr.setImageViolations(
          ((List<String>) solrDocument.getFieldValue(VendorProductSolrFieldNames.IMAGE_VIOLATIONS)).stream()
              .collect(Collectors.joining(SolrConstants.COMMA)));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(VendorProductSolrFieldNames.QC_RETRY_COUNT))) {
      vendorProductSolr.setQcRetryCount((int) solrDocument.getFieldValue(VendorProductSolrFieldNames.QC_RETRY_COUNT));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(VendorProductSolrFieldNames.PREDICTED_BRAND))) {
      vendorProductSolr
          .setPredictedBrand(String.valueOf(solrDocument.getFieldValue(VendorProductSolrFieldNames.PREDICTED_BRAND)));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(VendorProductSolrFieldNames.PRODUCT_REVIEW_TYPE))) {
      vendorProductSolr.setRevised(ProductReviewType.REVISED.name().equals(
        ProductReviewType.getProductReviewTypeByValue(String.valueOf(
          solrDocument.getFieldValue(VendorProductSolrFieldNames.PRODUCT_REVIEW_TYPE)))));
      vendorProductSolr.setEdited(ProductReviewType.EDITED.name().equals(
        ProductReviewType.getProductReviewTypeByValue(String.valueOf(
          solrDocument.getFieldValue(VendorProductSolrFieldNames.PRODUCT_REVIEW_TYPE)))));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(VendorProductSolrFieldNames.SELLER_TYPE))) {
      vendorProductSolr.setSellerType(SellerType.getSellerTypeByValue(
        (int) solrDocument.getFieldValue(VendorProductSolrFieldNames.SELLER_TYPE)));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(VendorProductSolrFieldNames.RESTRICTED_KEYWORDS_PRESENT))) {
      vendorProductSolr.setRestrictedKeywordsPresent(
        (boolean) solrDocument.getFieldValue(VendorProductSolrFieldNames.RESTRICTED_KEYWORDS_PRESENT));
    }
    List<Long> sellerBadgeValues = (List<Long>) solrDocument.getFieldValue(VendorProductSolrFieldNames.SELLER_BADGE);
    if (Objects.nonNull(sellerBadgeValues) && !sellerBadgeValues.isEmpty()) {
      Long sellerBadgeValue = sellerBadgeValues.get(0);
      vendorProductSolr.setSellerBadge(SellerBadge.getSellerBadgeByValue(sellerBadgeValue.intValue()));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(VendorProductSolrFieldNames.REVIEW_TYPE))) {
      vendorProductSolr.setReviewType(
          ReviewType.getReviewTypeByValue((int) solrDocument.getFieldValue(VendorProductSolrFieldNames.REVIEW_TYPE)));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(VendorProductSolrFieldNames.APPEALED_PRODUCT))) {
      vendorProductSolr.setAppealedProduct(
          (boolean) solrDocument.getFieldValue(VendorProductSolrFieldNames.APPEALED_PRODUCT));
    }
    if (Objects.nonNull(
      solrDocument.getFieldValue(VendorProductSolrFieldNames.DISTRIBUTION_MAPPING_STATUS))) {
      vendorProductSolr.setDistributionMappingStatus((int) solrDocument.getFieldValue(
        VendorProductSolrFieldNames.DISTRIBUTION_MAPPING_STATUS));
    }
    if (Objects.nonNull(
        solrDocument.getFieldValue(VendorProductSolrFieldNames.PRODUCT_CREATION_TYPE))) {
      vendorProductSolr.setProductCreationType(String.valueOf(
          solrDocument.getFieldValue(VendorProductSolrFieldNames.PRODUCT_CREATION_TYPE)));
    }
    List<String> productChannel = (List<String>) solrDocument.getFieldValue(VendorProductSolrFieldNames.PRODUCT_CHANNEL);
    if (Objects.nonNull(productChannel)) {
      vendorProductSolr.setProductChannel(productChannel);
    }
    return vendorProductSolr;
  }

  public static Product toProduct(VendorProductSolr vendorProductSolr) {
    return setProductInfo(vendorProductSolr);
  }

  private static Product setProductInfo(VendorProductSolr vendorProductSolr) {
    Product product = new Product();
    BeanUtils.copyProperties(vendorProductSolr, product, "sellerType", "productChannel");
    if (StringUtils.isNotEmpty(vendorProductSolr.getSellerType())) {
      product.setSellerType(SellerType.valueOf(vendorProductSolr.getSellerType()));
    }
    if(StringUtils.isNotEmpty(vendorProductSolr.getSellerBadge())){
      product.setSellerBadge(SellerBadgeConstants.fromSellerBadgeConstants(
        vendorProductSolr.getSellerBadge()));
    }
    product.setRestrictedKeywordsPresent(vendorProductSolr.isRestrictedKeywordsPresent());
    if (!SolrConstants.NOT_APPLICABLE.equals(vendorProductSolr.getVendorCode())) {
      product.setCurrentVendor(new Vendor());
      product.getCurrentVendor().setVendorCode(vendorProductSolr.getVendorCode());
    }
    product.setCategoryCode(vendorProductSolr.getCategoryCodes().get(0));
    product.setCategoryName(vendorProductSolr.getCategoryNames().get(0));
    product.setImageViolations(
        SolrConstants.NOT_APPLICABLE.equals(vendorProductSolr.getImageViolations()) ?
            null :
            vendorProductSolr.getImageViolations());
    product.setB2cActivated(
        Optional.ofNullable(vendorProductSolr.getProductChannel()).orElse(new ArrayList<>()).contains(Constants.RETAIL));
    product.setB2bActivated(
        Optional.ofNullable(vendorProductSolr.getProductChannel()).orElse(new ArrayList<>()).contains(Constants.B2B));
    product.setProductCreationType(vendorProductSolr.getProductCreationType());
    populateC1CategoryInfo(product, vendorProductSolr);
    return product;
  }

  public static ProductAndReviewerDetailsDTO toProductAndReviewerDetailsDTO(VendorProductSolr vendorProductSolr) {
    return new ProductAndReviewerDetailsDTO(setProductInfo(vendorProductSolr), setReviewerInfo(vendorProductSolr));
  }

  private static ProductReviewer setReviewerInfo(VendorProductSolr vendorProductSolr) {
    ProductReviewer productReviewer = ProductReviewer.builder().productCode(vendorProductSolr.getProductCode())
        .approvedDate(vendorProductSolr.getProductApprovedDate()).approverAssignee(
            SolrConstants.NOT_APPLICABLE.equals(vendorProductSolr.getProductApproverAssignee()) ?
                null :
                vendorProductSolr.getProductApproverAssignee()).assignedDate(vendorProductSolr.getProductAssignedDate()).build();
    return productReviewer;
  }

  public static VendorProductSolr toVendorProductSolr(Product product, ProductReviewer productReviewer,
    ImageQcProcessedAndBrandResponse imageQcProcessedAndBrandResponse) {
    VendorProductSolr vendorProductSolr = new VendorProductSolr();
    BeanUtils.copyProperties(product, vendorProductSolr);
    if (Objects.nonNull(product.getCurrentVendor())) {
      vendorProductSolr.setVendorCode(product.getCurrentVendor().getVendorCode());
    } else {
      vendorProductSolr.setVendorCode(SolrConstants.NOT_APPLICABLE);
    }
    vendorProductSolr.setCategoryCodes(Arrays.asList(product.getCategoryCode()));
    vendorProductSolr.setCategoryNames(Arrays.asList(product.getCategoryName()));
    vendorProductSolr.setProductApproverAssignee(StringUtils.isNotEmpty(productReviewer.getApproverAssignee()) ?
        productReviewer.getApproverAssignee() :
        SolrConstants.NOT_APPLICABLE);
    if (Objects.nonNull(productReviewer.getAssignedDate())) {
      vendorProductSolr.setProductAssignedDate(productReviewer.getAssignedDate());
    }
    if (Objects.nonNull(productReviewer.getApprovedDate())) {
      vendorProductSolr.setProductApprovedDate(productReviewer.getApprovedDate());
    }
    if (Objects.nonNull(product.getReviewType())) {
      vendorProductSolr.setReviewType(product.getReviewType().name());
    }
    if (Optional.ofNullable(imageQcProcessedAndBrandResponse)
      .map(ImageQcProcessedAndBrandResponse::getImageQcProcessedResponse).isPresent()
      && Boolean.TRUE.equals(product.isPostLive()) &&
      !SellerType.TRUSTED_SELLER.equals(product.getSellerType())) {
      vendorProductSolr.setPostLive(
        !imageQcProcessedAndBrandResponse.getImageQcProcessedResponse().isForceReview());
    }
    vendorProductSolr.setImageViolations(setImageViolations(product));
    if(Objects.nonNull(product.getSellerType())){
      vendorProductSolr.setSellerType(product.getSellerType().name());
    }
    if (Objects.nonNull(product.getSellerBadge())){
      vendorProductSolr.setSellerBadge(product.getSellerBadge().name());
    }
    return vendorProductSolr;
  }

  private static String setImageViolations(Product product) {
    List<String> violations = new ArrayList<>();
    if (StringUtils.isNotEmpty(product.getTextViolations())) {
      violations = new ArrayList<>(Arrays.asList(product.getTextViolations().split(SolrConstants.COMMA)));
    }
    if (StringUtils.isNotEmpty(product.getImageViolations())) {
      violations.addAll(Arrays.asList(product.getImageViolations().split(SolrConstants.COMMA)));
    }
    return String.join(SolrConstants.COMMA, new HashSet<>(violations));
  }

  public static List<SolrInputDocument> getInputForVendorMappingAtomicUpdate(String vendorCode,
      List<String> productCodeList) {
    List<SolrInputDocument> solrInputDocumentList = new ArrayList<>();
    for (String productCode : productCodeList) {
      SolrInputDocument solrInputDocument = new SolrInputDocument();
      solrInputDocument.setField(VendorProductSolrFieldNames.PRODUCT_CODE, productCode);
      solrInputDocument.setField(VendorProductSolrFieldNames.UPDATED_DATE,
          Collections.singletonMap(SolrConstants.SET_CLAUSE, new Date()));
      solrInputDocument.setField(VendorProductSolrFieldNames.VENDOR_CODE,
          Collections.singletonMap(SolrConstants.SET_CLAUSE, vendorCode));
      solrInputDocumentList.add(solrInputDocument);
    }
    return solrInputDocumentList;
  }

  public static SolrInputDocument getInputForReviewerUpdate(String productCode, String assignedTo, Date assignedDate) {
    SolrInputDocument solrInputDocument = new SolrInputDocument();
    solrInputDocument.setField(VendorProductSolrFieldNames.PRODUCT_CODE, productCode);
    solrInputDocument.setField(VendorProductSolrFieldNames.UPDATED_DATE,
        Collections.singletonMap(SolrConstants.SET_CLAUSE, new Date()));
    boolean assignee = true;
    if (StringUtils.isEmpty(assignedTo)) {
      assignedTo = SolrConstants.NOT_APPLICABLE;
      assignedDate = null;
      assignee = false;
    }
    solrInputDocument.setField(VendorProductSolrFieldNames.ASSIGNED_DATE,
        Collections.singletonMap(SolrConstants.SET_CLAUSE, assignedDate));
    solrInputDocument.setField(VendorProductSolrFieldNames.APPROVER_ASSIGNEE,
        Collections.singletonMap(SolrConstants.SET_CLAUSE, assignedTo));
    solrInputDocument.setField(VendorProductSolrFieldNames.ASSIGNED,
        Collections.singletonMap(SolrConstants.SET_CLAUSE, assignee));
    return solrInputDocument;
  }

  public static SolrQuery getQueryForReviewConfigCounts(String storeId, String vendorCode) {
    SolrQuery solrQuery = new SolrQuery();
    solrQuery.addFilterQuery(
        new StringBuilder().append(VendorProductSolrFieldNames.STORE_ID).append(SolrConstants.COLON).append(storeId)
            .toString());
    solrQuery.addFilterQuery(
        new StringBuilder().append(VendorProductSolrFieldNames.VENDOR_CODE).append(SolrConstants.COLON)
            .append(vendorCode).toString());
    solrQuery.setStart(0);
    solrQuery.setRows(1);
    solrQuery.set(CommonParams.TZ, SolrConstants.JKT_TIME_ZONE);
    solrQuery.setFacet(true);
    solrQuery.addFacetField(VendorProductSolrFieldNames.POST_LIVE);
    solrQuery.setQuery(
        new StringBuilder().append(SolrConstants.NOT).append(VendorProductSolrFieldNames.STATE)
            .append(SolrConstants.COLON).append(SolrConstants.OPEN_BRACKET)
            .append(WorkflowState.PASSED).append(SolrConstants.CLOSE_BRACKET)
            .toString());
    return solrQuery;
  }

  public static SolrQuery getQueryForReviewCountsForConfig(String storeId, String vendorCode, boolean postLive) {
    SolrQuery solrQuery = new SolrQuery();
    solrQuery.addFilterQuery(
        new StringBuilder().append(VendorProductSolrFieldNames.STORE_ID).append(SolrConstants.COLON).append(storeId)
            .toString());
    solrQuery.addFilterQuery(
        new StringBuilder().append(VendorProductSolrFieldNames.VENDOR_CODE).append(SolrConstants.COLON)
            .append(vendorCode).toString());
    solrQuery.addFilterQuery(
        new StringBuilder().append(VendorProductSolrFieldNames.POST_LIVE).append(SolrConstants.COLON).append(postLive)
            .toString());
    solrQuery.addFilterQuery(getQueryForMarkForDelete(false));
    solrQuery.setStart(0);
    solrQuery.setRows(1);
    solrQuery.set(CommonParams.TZ, SolrConstants.JKT_TIME_ZONE);
    solrQuery.setFacet(true);
    solrQuery.addFacetField(VendorProductSolrFieldNames.PRODUCT_REVIEW_TYPE);
    solrQuery.setQuery(
      new StringBuilder().append(SolrConstants.NOT).append(VendorProductSolrFieldNames.STATE)
        .append(SolrConstants.COLON).append(SolrConstants.OPEN_BRACKET).append(WorkflowState.PASSED)
        .append(SolrConstants.CLOSE_BRACKET).toString());
    return solrQuery;
  }

  public static SolrInputDocument getInputForImageQcResponse(Product product, ProductReviewer productReviewer) {
    SolrInputDocument solrInputDocument = new SolrInputDocument();
    solrInputDocument.setField(VendorProductSolrFieldNames.PRODUCT_CODE, product.getProductCode());
    solrInputDocument.setField(VendorProductSolrFieldNames.UPDATED_DATE,
        Collections.singletonMap(SolrConstants.SET_CLAUSE, new Date()));
    if (Objects.nonNull(productReviewer)) {
      if (Objects.nonNull(productReviewer.getAssignedDate())) {
        solrInputDocument.setField(VendorProductSolrFieldNames.ASSIGNED_DATE,
            Collections.singletonMap(SolrConstants.SET_CLAUSE, productReviewer.getAssignedDate()));
      }
      boolean assignee = false;
      if (Objects.nonNull(productReviewer.getApproverAssignee())) {
        solrInputDocument.setField(VendorProductSolrFieldNames.APPROVER_ASSIGNEE,
            Collections.singletonMap(SolrConstants.SET_CLAUSE, productReviewer.getApproverAssignee()));
        assignee = true;
      } if (Objects.nonNull(productReviewer.getApprovedDate())) {
        solrInputDocument.setField(VendorProductSolrFieldNames.APPROVED_DATE,
            Collections.singletonMap(SolrConstants.SET_CLAUSE, productReviewer.getApprovedDate()));
      }
      solrInputDocument.setField(VendorProductSolrFieldNames.ASSIGNED,
          Collections.singletonMap(SolrConstants.SET_CLAUSE, assignee));
    }
    String violations = setImageViolations(product);
    solrInputDocument.setField(VendorProductSolrFieldNames.IMAGE_VIOLATIONS,
        Collections.singletonMap(SolrConstants.SET_CLAUSE, StringUtils.isNotEmpty(violations) ?
            Arrays.asList(violations.split(",")) :
            Collections.singletonList(SolrConstants.NOT_APPLICABLE)));
    solrInputDocument.setField(VendorProductSolrFieldNames.STATE,
        Collections.singletonMap(SolrConstants.SET_CLAUSE, product.getState().name()));
    solrInputDocument.setField(VendorProductSolrFieldNames.POST_LIVE,
      (product.isPostLive() && !product.isForceReview()));
    solrInputDocument.setField(VendorProductSolrFieldNames.PREDICTED_BRAND, product.getPredictedBrand());
    return solrInputDocument;
  }

  public static SolrQuery getSolrQueryForFilterCounts(String storeId, String vendorCode, Boolean edited,
      Boolean postLive, Boolean revised) {
    List<String> facetFields = new ArrayList<>();
    SolrQuery solrQuery = new SolrQuery();
    solrQuery.setQuery(getQueryString());
    solrQuery.addFilterQuery(
        VendorProductSolrFieldNames.VENDOR_CODE + SolrConstants.COLON + appendDoubleQuotes(vendorCode));
    solrQuery.addFilterQuery(VendorProductSolrFieldNames.STORE_ID + SolrConstants.COLON + appendDoubleQuotes(storeId));
    if (Objects.nonNull(postLive)) {
      solrQuery.addFilterQuery(
        VendorProductSolrFieldNames.POST_LIVE + SolrConstants.COLON + postLive);
    }
    else {
      // include both post live and pre live products
      solrQuery.addFilterQuery(
        VendorProductSolrFieldNames.POST_LIVE + SolrConstants.COLON + SolrConstants.OPEN_BRACKET
          + String.join(SolrConstants.OR, String.valueOf(Boolean.TRUE),
          String.valueOf(Boolean.FALSE)) + SolrConstants.CLOSE_BRACKET);
    }
    facetFields.add(VendorProductSolrFieldNames.BRAND_APPROVAL_STATUS);
    String productReviewType = getProductReviewTypeForFilterCount(edited,revised);
    if(StringUtils.isNotEmpty(productReviewType)){
      solrQuery.addFilterQuery(
        VendorProductSolrFieldNames.PRODUCT_REVIEW_TYPE + SolrConstants.COLON + productReviewType);
    }
    if (revisedDataNeeded(edited, revised)) {
      facetFields.add(VendorProductSolrFieldNames.REVIEW_TYPE);
    }
    //Add facet for restricted keyword for All tabs and Pre Live tab
    if (Objects.isNull(postLive) || Boolean.FALSE.equals(postLive)) {
      facetFields.add(VendorProductSolrFieldNames.RESTRICTED_KEYWORDS_PRESENT);
    }
    solrQuery.addFilterQuery(VendorProductSolrFieldNames.MARK_FOR_DELETE + SolrConstants.COLON + false);
    solrQuery.setStart(0);
    solrQuery.setRows(1);
    solrQuery.setFacet(Boolean.TRUE);
    solrQuery.set(CommonParams.TZ, SolrConstants.JKT_TIME_ZONE);
    facetFields.add(VendorProductSolrFieldNames.ASSIGNED);
    solrQuery.addFacetField(Arrays.copyOf(facetFields.toArray(), facetFields.toArray().length, String[].class));
    solrQuery.addIntervalFacets(VendorProductSolrFieldNames.PRODUCT_CREATED_DATE,
        new String[] {SolrConstants.TODAY_FACET_INTERVAL, SolrConstants.YESTERDAY_FACET_INTERVAL,
            SolrConstants.TWO_DAYS_AGO_FACET_INTERVAL, SolrConstants.THREE_TO_FIVE_DAYS_AGO_FACET_INTERVAL,
            SolrConstants.FIVE_DAYS_AGO_FACET_INTERVAL});
    return solrQuery;
  }

  private static boolean revisedDataNeeded(Boolean edited, Boolean revised) {
    return (Objects.isNull(edited) && Objects.isNull(revised)) || (Boolean.TRUE.equals(edited)
      || Boolean.TRUE.equals(revised));
  }

  public static SolrQuery getSolrQueryForFinalQcCounts(String storeId) {
    SolrQuery solrQuery = new SolrQuery();
    solrQuery.setQuery(getFinalQcQueryString());
    solrQuery.addFilterQuery(VendorProductSolrFieldNames.STORE_ID + SolrConstants.COLON + appendDoubleQuotes(storeId));
    solrQuery.addFilterQuery(VendorProductSolrFieldNames.MARK_FOR_DELETE + SolrConstants.COLON + false);
    solrQuery.setStart(0);
    solrQuery.setRows(1);
    solrQuery.setFields(VendorProductSolrFieldNames.PRODUCT_CODE);
    solrQuery.setFacet(Boolean.TRUE);
    solrQuery.set(CommonParams.TZ, SolrConstants.JKT_TIME_ZONE);
    solrQuery.addIntervalFacets(VendorProductSolrFieldNames.PRODUCT_CREATED_DATE,
        new String[] {SolrConstants.TODAY_FACET_INTERVAL, SolrConstants.YESTERDAY_FACET_INTERVAL,
            SolrConstants.TWO_TO_THREE_DAYS_AGO_FACET_INTERVAL,
            SolrConstants.THREE_DAYS_AGO_FACET_INTERVAL});
    return solrQuery;
  }

  public static SolrQuery getSolrQueryForDistributionListCounts(String storeId) {
    SolrQuery solrQuery = new SolrQuery();
    solrQuery.setQuery(VendorProductSolrFieldNames.STORE_ID + SolrConstants.COLON + appendDoubleQuotes(storeId));
    solrQuery.addFilterQuery(VendorProductSolrFieldNames.MARK_FOR_DELETE + SolrConstants.COLON + false);
    solrQuery.setStart(0);
    solrQuery.setRows(1);
    solrQuery.setFacet(Boolean.TRUE);
    solrQuery.setFields(VendorProductSolrFieldNames.PRODUCT_CODE);
    solrQuery.set(CommonParams.TZ, SolrConstants.JKT_TIME_ZONE);
    String[] facetFields = new String[] {VendorProductSolrFieldNames.STATE};
    solrQuery.addFacetField(facetFields);
    solrQuery.addIntervalFacets(VendorProductSolrFieldNames.PRODUCT_CREATED_DATE,
        new String[] {SolrConstants.TODAY_FACET_INTERVAL, SolrConstants.YESTERDAY_FACET_INTERVAL,
            SolrConstants.TWO_TO_THREE_DAYS_AGO_FACET_INTERVAL,
            SolrConstants.THREE_DAYS_AGO_FACET_INTERVAL});
    return solrQuery;
  }


  public static SolrQuery getSolrQueryForBusinessPartnerList(String storeId, PrimaryFilterDTO primaryFilterDTO,
      List<String> stateList) {
    SolrQuery solrQuery = new SolrQuery();
    StringBuilder query = new StringBuilder(VendorProductSolrFieldNames.STATE).append(SolrConstants.COLON)
        .append(SolrConstants.OPEN_BRACKET).append(SolrConstants.DOUBLE_QUOTE)
        .append(String.join(SolrConstants.COMMA_WITH_QUOTES, stateList)).append(SolrConstants.DOUBLE_QUOTE)
        .append(SolrConstants.CLOSE_BRACKET);
    if (Objects.nonNull(primaryFilterDTO.getPostLive())) {
      solrQuery.addFilterQuery(
          VendorProductSolrFieldNames.POST_LIVE + SolrConstants.COLON + primaryFilterDTO.getPostLive());
    }
    solrQuery.addFilterQuery(VendorProductSolrFieldNames.MARK_FOR_DELETE + SolrConstants.COLON + false);
    solrQuery.addFilterQuery(
        VendorProductSolrFieldNames.PRODUCT_REVIEW_TYPE + SolrConstants.COLON + getProductReviewType(
            primaryFilterDTO.isEdited(), primaryFilterDTO.isRevised()));
    if (Objects.nonNull(primaryFilterDTO.getContentPending()) || Objects.nonNull(primaryFilterDTO.getImagePending())) {
      solrQuery.addFilterQuery(VendorProductSolrFieldNames.REVIEW_TYPE + SolrConstants.COLON + getReviewTypeFromRequest(
          primaryFilterDTO.getContentPending(), primaryFilterDTO.getImagePending()));
    }
    solrQuery.addFilterQuery(VendorProductSolrFieldNames.STORE_ID + SolrConstants.COLON + storeId);
    solrQuery.addFilterQuery(VendorProductSolrFieldNames.VENDOR_CODE + SolrConstants.COLON + appendDoubleQuotes(
        primaryFilterDTO.getVendorCode()));
    solrQuery.addFilterQuery(SolrConstants.COLLAPSE_FQ_FOR_BP_CODE);
    solrQuery.setSort(VendorProductSolrFieldNames.COPY_BUSINESS_PARTNER_NAME, SolrQuery.ORDER.asc);
    solrQuery.set(CommonParams.TZ, SolrConstants.JKT_TIME_ZONE);
    String timeFilterQuery =
        VendorProductSolrHelper.getQueryForProductCreatedDate(primaryFilterDTO.getTimeFilterType());
    if (!TimeFilterType.ALL.equals(primaryFilterDTO.getTimeFilterType()) && EnumUtils.isValidEnum(TimeFilterType.class,
        primaryFilterDTO.getTimeFilterType().name())) {
      solrQuery.addFilterQuery(
          VendorProductSolrFieldNames.PRODUCT_CREATED_DATE + SolrConstants.COLON + timeFilterQuery);
    }
    if (Objects.nonNull(primaryFilterDTO.getBrandPending()) && Boolean.TRUE
        .equals(primaryFilterDTO.getBrandPending())) {
      solrQuery.addFilterQuery(
          SolrConstants.NOT_IN + VendorProductSolrFieldNames.BRAND_APPROVAL_STATUS + SolrConstants.COLON
              + BrandApprovalStatus.APPROVED.getValue());
    }
    if (Objects.nonNull(primaryFilterDTO.getRestrictedKeyword())) {
      solrQuery.addFilterQuery(
          VendorProductSolrFieldNames.RESTRICTED_KEYWORDS_PRESENT + SolrConstants.COLON + primaryFilterDTO
              .getRestrictedKeyword());
    }
    if (Objects.nonNull(primaryFilterDTO.getAssignment())) {
      solrQuery.addFilterQuery(
          VendorProductSolrFieldNames.ASSIGNED + SolrConstants.COLON + primaryFilterDTO.getAssignment());
    }
    setKeywordFilter(primaryFilterDTO, query);
    log.info("Solr query to get BusinessPartnerList: {} ", query);
    solrQuery.setQuery(query.toString());
    return solrQuery;
  }

  public static SolrInputDocument getSolrInputDocumentOnContentApprovalOrSave(Product product, ProductReviewer productReviewer,
      List<CategoryResponse> categoryResponses) {
    SolrInputDocument solrInputDocument = new SolrInputDocument();
    solrInputDocument.setField(VendorProductSolrFieldNames.PRODUCT_CODE, product.getProductCode());
    solrInputDocument
        .setField(VendorProductSolrFieldNames.STATE, Collections.singletonMap(SolrConstants.SET, product.getState().name()));
    solrInputDocument
        .setField(VendorProductSolrFieldNames.BRAND, Collections.singletonMap(SolrConstants.SET, product.getBrand()));
    solrInputDocument.setField(VendorProductSolrFieldNames.PRODUCT_NAME,
        Collections.singletonMap(SolrConstants.SET, product.getProductName()));
    if (StringUtils.isNotEmpty(product.getBrandApprovalStatus())) {
      solrInputDocument.setField(VendorProductSolrFieldNames.BRAND_APPROVAL_STATUS,
          Collections.singletonMap(SolrConstants.SET,
              (BrandApprovalStatus.valueOf(product.getBrandApprovalStatus())).getValue()));
    }
    solrInputDocument.setField(VendorProductSolrFieldNames.APPROVED_DATE,
        productReviewer.getApprovedDate());
    solrInputDocument
        .setField(VendorProductSolrFieldNames.UPDATED_DATE, Collections.singletonMap(SolrConstants.SET, new Date()));
    if (CollectionUtils.isNotEmpty(categoryResponses)) {
      solrInputDocument.setField(VendorProductSolrFieldNames.CATEGORY_CODES, Collections.singletonMap(SolrConstants.SET,
          categoryResponses.stream().map(CategoryResponse::getCategoryCode).collect(Collectors.toList())));
      solrInputDocument.setField(VendorProductSolrFieldNames.CATEGORY_NAMES, Collections.singletonMap(SolrConstants.SET,
          categoryResponses.stream().map(CategoryResponse::getName).collect(Collectors.toList())));
    }
    return solrInputDocument;
  }

  public static SolrInputDocument getSolrInputDocumentOnApprovalOrSave(PDTProductUpdateProductToSolrEventModel product,
      List<CategoryResponse> categoryResponses) {
    SolrInputDocument solrInputDocument = new SolrInputDocument();
    solrInputDocument.setField(VendorProductSolrFieldNames.PRODUCT_CODE, product.getProductCode());
    solrInputDocument.setField(VendorProductSolrFieldNames.STATE,
        Collections.singletonMap(SolrConstants.SET, product.getState()));
    solrInputDocument.setField(VendorProductSolrFieldNames.BRAND,
        Collections.singletonMap(SolrConstants.SET, product.getBrand()));
    solrInputDocument.setField(VendorProductSolrFieldNames.PRODUCT_NAME,
        Collections.singletonMap(SolrConstants.SET, product.getProductName()));
    solrInputDocument.setField(VendorProductSolrFieldNames.BRAND_APPROVAL_STATUS,
        Collections.singletonMap(SolrConstants.SET, (BrandApprovalStatus.valueOf(
            StringUtils.isNotBlank(product.getBrandApprovalStatus()) ?
                product.getBrandApprovalStatus() :
                BrandApprovalStatus.APPROVED.name())).getValue()));
    solrInputDocument.setField(VendorProductSolrFieldNames.APPROVED_DATE, product.getApprovedDate());
    solrInputDocument.setField(VendorProductSolrFieldNames.UPDATED_DATE,
        Collections.singletonMap(SolrConstants.SET, new Date()));
    solrInputDocument.setField(VendorProductSolrFieldNames.REVIEW_TYPE, product.getReviewType());
    solrInputDocument.setField(VendorProductSolrFieldNames.SELLER_TYPE, product.getSellerType());
    solrInputDocument.setField(VendorProductSolrFieldNames.POST_LIVE, (product.isPostLive()));
    if (CollectionUtils.isNotEmpty(categoryResponses)) {
      solrInputDocument.setField(VendorProductSolrFieldNames.CATEGORY_CODES, Collections.singletonMap(SolrConstants.SET,
          categoryResponses.stream().map(CategoryResponse::getCategoryCode).collect(Collectors.toList())));
      solrInputDocument.setField(VendorProductSolrFieldNames.CATEGORY_NAMES, Collections.singletonMap(SolrConstants.SET,
          categoryResponses.stream().map(CategoryResponse::getName).collect(Collectors.toList())));
    }
    SellerBadge sellerBadge = SellerBadge.NONE_MERCHANT;
    if (StringUtils.isNotEmpty(product.getSellerBadge())) {
      sellerBadge = SellerBadge.valueOf(product.getSellerBadge());
    }
    solrInputDocument.setField(VendorProductSolrFieldNames.SELLER_BADGE,
      List.of(sellerBadge.getValue()));
    solrInputDocument.setField(VendorProductSolrFieldNames.APPEALED_PRODUCT,
      product.isAppealedProduct());
    solrInputDocument.setField(VendorProductSolrFieldNames.DISTRIBUTION_MAPPING_STATUS,
      product.getDistributionMappingStatus());
    solrInputDocument.setField(VendorProductSolrFieldNames.PRODUCT_CREATION_TYPE,
        product.getProductCreationType());
    return solrInputDocument;
  }

  public static SolrInputDocument getSolrInputDocumentOnImageApproval(Product product, ProductReviewer productReviewer) {
    SolrInputDocument solrInputDocument = new SolrInputDocument();
    solrInputDocument.setField(VendorProductSolrFieldNames.PRODUCT_CODE, product.getProductCode());
    solrInputDocument
        .setField(VendorProductSolrFieldNames.STATE, Collections.singletonMap(SolrConstants.SET, product.getState().name()));
    solrInputDocument.setField(VendorProductSolrFieldNames.APPROVED_DATE,
        productReviewer.getApprovedDate());
    solrInputDocument
        .setField(VendorProductSolrFieldNames.UPDATED_DATE, Collections.singletonMap(SolrConstants.SET, new Date()));
    return solrInputDocument;
  }

  private static void setKeywordFilter(PrimaryFilterDTO primaryFilterDTO, StringBuilder query) {
    if (StringUtils.isNotEmpty(primaryFilterDTO.getKeyword()) && !SolrConstants.EXTERNAL.equals(
        primaryFilterDTO.getKeyword())) {
      query.append(SolrConstants.AND).append(VendorProductSolrFieldNames.BUSINESS_PARTNER_NAME)
          .append(SolrConstants.COLON).append(SolrConstants.OPEN_BRACKET)
          .append(getSearchKeywordWithoutFuzzySearch(primaryFilterDTO.getKeyword().toLowerCase()))
          .append(SolrConstants.LIKE_QUERY).append(SolrConstants.CLOSE_BRACKET).append(SolrConstants.AND)
          .append(SolrConstants.NOT_IN).append(VendorProductSolrFieldNames.BUSINESS_PARTNER_NAME)
          .append(SolrConstants.COLON).append(SolrConstants.OPEN_BRACKET).append(SolrConstants.INTERNAL)
          .append(SolrConstants.CLOSE_BRACKET);
    } else if (StringUtils.isNotEmpty(primaryFilterDTO.getKeyword())) {
      query.append(SolrConstants.AND).append(SolrConstants.NOT_IN)
          .append(VendorProductSolrFieldNames.BUSINESS_PARTNER_NAME).append(SolrConstants.COLON)
          .append(SolrConstants.OPEN_BRACKET).append(SolrConstants.INTERNAL).append(SolrConstants.CLOSE_BRACKET);
    }
  }

  private static String getSearchKeywordWithoutFuzzySearch(String searchKeyword) {
    return String.join(SolrConstants.AND, StringUtils.trimToEmpty(searchKeyword).split(REGEX_FOR_SPACE));
  }

  private static String getQueryString() {
    return new StringBuilder(SolrConstants.NOT_IN).append(VendorProductSolrFieldNames.STATE)
        .append(SolrConstants.COLON).append(SolrConstants.OPEN_BRACKET)
        .append(appendDoubleQuotes(WorkflowState.PASSED.name()))
        .append(SolrConstants.CLOSE_BRACKET).toString();
  }

  private static String getFinalQcQueryString() {
    return new StringBuilder(VendorProductSolrFieldNames.STATE).append(SolrConstants.COLON)
        .append(SolrConstants.OPEN_BRACKET).append(appendDoubleQuotes(WorkflowState.PASSED.name()))
        .append(SolrConstants.CLOSE_BRACKET).toString();
  }

  public static SolrInputDocument getSolrInputDocumentOnBrandApprovalOrRejection(String productCode, String brand,
      String brandStatus) {
    SolrInputDocument solrInputDocument = new SolrInputDocument();
    solrInputDocument.setField(VendorProductSolrFieldNames.PRODUCT_CODE, productCode);
    solrInputDocument.setField(VendorProductSolrFieldNames.BRAND, Collections.singletonMap(SolrConstants.SET, brand));
    solrInputDocument.setField(VendorProductSolrFieldNames.BRAND_APPROVAL_STATUS,
      Collections.singletonMap(SolrConstants.SET,
        BrandApprovalStatus.valueOf(brandStatus).getValue()));
    return solrInputDocument;
  }

  public static SolrInputDocument getSolrInputDocumentForClearReviewerDetailsAndUpdateState(String productCode, WorkflowState state) {
    SolrInputDocument solrInputDocument = new SolrInputDocument();
    solrInputDocument.setField(VendorProductSolrFieldNames.PRODUCT_CODE, productCode);
    solrInputDocument.setField(VendorProductSolrFieldNames.UPDATED_DATE,
        Collections.singletonMap(SolrConstants.SET_CLAUSE, new Date()));
    solrInputDocument.setField(VendorProductSolrFieldNames.ASSIGNED_DATE,
        Collections.singletonMap(SolrConstants.SET_CLAUSE, null));
    solrInputDocument.setField(VendorProductSolrFieldNames.APPROVER_ASSIGNEE,
        Collections.singletonMap(SolrConstants.SET_CLAUSE, SolrConstants.NOT_APPLICABLE));
    solrInputDocument.setField(VendorProductSolrFieldNames.APPROVED_DATE,
        Collections.singletonMap(SolrConstants.SET_CLAUSE, null));
    solrInputDocument.setField(VendorProductSolrFieldNames.ASSIGNED,
        Collections.singletonMap(SolrConstants.SET_CLAUSE, false));
    solrInputDocument.setField(VendorProductSolrFieldNames.STATE,
        Collections.singletonMap(SolrConstants.SET_CLAUSE, state.toString()));
    return solrInputDocument;
  }

  public static SolrInputDocument getSolrInputDocumentForProductRetryUpdate(Product updateProduct) {
    SolrInputDocument solrInputDocument = new SolrInputDocument();
    solrInputDocument.setField(VendorProductSolrFieldNames.PRODUCT_CODE, updateProduct.getProductCode());
    solrInputDocument.setField(VendorProductSolrFieldNames.UPDATED_DATE,
      Collections.singletonMap(SolrConstants.SET_CLAUSE, new Date()));
    solrInputDocument.setField(VendorProductSolrFieldNames.STATE,
      Collections.singletonMap(SolrConstants.SET_CLAUSE, updateProduct.getState().toString()));
    solrInputDocument.setField(VendorProductSolrFieldNames.PRODUCT_REVIEW_TYPE,
      Collections.singletonMap(SolrConstants.SET_CLAUSE, getProductReviewType(
        updateProduct.isEdited(), updateProduct.isRevised())));
    solrInputDocument.setField(VendorProductSolrFieldNames.REVIEW_TYPE,
      Collections.singletonMap(SolrConstants.SET_CLAUSE, updateProduct.getReviewType().getValue()));
    return solrInputDocument;
  }

  public static SolrInputDocument getSolrInputDocumentForUpdateState(String productCode,
      WorkflowState stateForProduct) {
    SolrInputDocument solrInputDocument = new SolrInputDocument();
    solrInputDocument.setField(VendorProductSolrFieldNames.PRODUCT_CODE, productCode);
    solrInputDocument.setField(VendorProductSolrFieldNames.UPDATED_DATE,
        Collections.singletonMap(SolrConstants.SET_CLAUSE, new Date()));
    solrInputDocument.setField(VendorProductSolrFieldNames.STATE,
        Collections.singletonMap(SolrConstants.SET_CLAUSE, stateForProduct.toString()));
    return solrInputDocument;
  }

  public static SolrInputDocument getSolrInputDocumentForUpdateStateAndMfdTrue(String productCode,
      WorkflowState stateForProduct) {
    SolrInputDocument solrInputDocument = new SolrInputDocument();
    solrInputDocument.setField(VendorProductSolrFieldNames.PRODUCT_CODE, productCode);
    solrInputDocument.setField(VendorProductSolrFieldNames.UPDATED_DATE,
        Collections.singletonMap(SolrConstants.SET_CLAUSE, new Date()));
    solrInputDocument.setField(VendorProductSolrFieldNames.STATE,
        Collections.singletonMap(SolrConstants.SET_CLAUSE, stateForProduct.toString()));
    solrInputDocument.setField(VendorProductSolrFieldNames.MARK_FOR_DELETE,
        Collections.singletonMap(SolrConstants.SET_CLAUSE, true));
    return solrInputDocument;
  }

  public static SolrInputDocument getInputForPostLiveFlagUpdate(String productCode,
      boolean postLive) {
    SolrInputDocument solrInputDocument = new SolrInputDocument();
    solrInputDocument.setField(VendorProductSolrFieldNames.PRODUCT_CODE, productCode);
    solrInputDocument.setField(VendorProductSolrFieldNames.UPDATED_DATE,
        Collections.singletonMap(SolrConstants.SET_CLAUSE, new Date()));
    solrInputDocument.setField(VendorProductSolrFieldNames.POST_LIVE,
        Collections.singletonMap(SolrConstants.SET_CLAUSE, postLive));
    return solrInputDocument;
  }

  public static SolrQuery getQueryForBusinessPartnerMapper(List<String> workflowStateList, String searchCriteria,
      Pageable pageable, String storeId) {
    SolrQuery solrQuery = new SolrQuery();
    solrQuery.setStart(pageable.getPageNumber() * pageable.getPageSize());
    solrQuery.setRows(pageable.getPageSize());
    solrQuery.setFields(VendorProductSolrFieldNames.BUSINESS_PARTNER_CODE,
        VendorProductSolrFieldNames.BUSINESS_PARTNER_NAME);
    solrQuery.addFilterQuery(VendorProductSolrFieldNames.MARK_FOR_DELETE + SolrConstants.COLON + false);
    solrQuery.addFilterQuery(VendorProductSolrFieldNames.STORE_ID + SolrConstants.COLON + storeId);
    solrQuery.addFilterQuery(SolrConstants.COLLAPSE_FQ_FOR_BP_CODE);
    solrQuery.setSort(VendorProductSolrFieldNames.COPY_BUSINESS_PARTNER_NAME, SolrQuery.ORDER.asc);
    StringBuilder query = new StringBuilder(VendorProductSolrFieldNames.STATE).append(SolrConstants.COLON)
        .append(SolrConstants.OPEN_BRACKET).append(SolrConstants.DOUBLE_QUOTE)
        .append(String.join(SolrConstants.COMMA_WITH_QUOTES, workflowStateList)).append(SolrConstants.DOUBLE_QUOTE)
        .append(SolrConstants.CLOSE_BRACKET);
    if (StringUtils.isNotBlank(searchCriteria)) {
      query.append(SolrConstants.AND).append(SolrConstants.OPEN_BRACKET)
          .append(getSearchQueryByBusinessName(searchCriteria)).append(SolrConstants.CLOSE_BRACKET);
    }
    solrQuery.setQuery(query.toString());
    return solrQuery;
  }

  private static StringBuilder getSearchQueryByBusinessName(String searchKeyword) {
    String keyword = getSearchKeyword(StringUtils.trimToEmpty(searchKeyword));
    String keywordWithoutSpecialChars =
        getSearchKeywordWithoutSpecialCharacters(new StringBuilder(StringUtils.trimToEmpty(searchKeyword)));
    StringBuilder query = new StringBuilder();
    if (StringUtils.isNotEmpty(searchKeyword)) {
      getKeywordQueryBusinessPartnerName(keyword, keywordWithoutSpecialChars, query);
    }
    return query;
  }

  private static void getKeywordQueryBusinessPartnerName(String keyword, String keywordWithoutSpecialChars,
      StringBuilder query) {
    query.append(SolrConstants.OPEN_BRACKET).append(VendorProductSolrFieldNames.BUSINESS_PARTNER_NAME)
        .append(SolrConstants.COLON).append(SolrConstants.OPEN_BRACKET).append(keywordWithoutSpecialChars)
        .append(SolrConstants.CLOSE_BRACKET).append(StringUtils.SPACE).append(SolrConstants.OR)
        .append(StringUtils.SPACE).append(VendorProductSolrFieldNames.BUSINESS_PARTNER_NAME).append(SolrConstants.COLON)
        .append(SolrConstants.OPEN_BRACKET).append(SolrConstants.DOUBLE_QUOTES).append(StringUtils.trimToEmpty(keyword))
        .append(SolrConstants.DOUBLE_QUOTES).append(SolrConstants.CLOSE_BRACKET).append(SolrConstants.CLOSE_BRACKET);
  }

  public static ProductBusinessPartnerMapper toProductBusinessPartnerList(SolrDocument solrDocument) {
    ProductBusinessPartnerMapper mapperResponse = new ProductBusinessPartnerMapper();
    mapperResponse.setBusinessPartnerCode(
        String.valueOf(solrDocument.getFieldValue(VendorProductSolrFieldNames.BUSINESS_PARTNER_CODE)));
    mapperResponse.setBusinessPartnerName(
        String.valueOf(solrDocument.getFieldValue(VendorProductSolrFieldNames.BUSINESS_PARTNER_NAME)));
    return mapperResponse;
  }

  public static SolrQuery getSolrQueryForCountVendorAssignment() {
    SolrQuery solrQuery = new SolrQuery();
    solrQuery.setQuery(VendorProductSolrFieldNames.VENDOR_CODE + SolrConstants.COLON + SolrConstants.LIKE_QUERY);
    solrQuery.addFilterQuery(VendorProductSolrFieldNames.MARK_FOR_DELETE + SolrConstants.COLON + Boolean.FALSE);
    solrQuery.setFields(VendorProductSolrFieldNames.PRODUCT_CODE);
    solrQuery.setRows(1);
    solrQuery.setStart(0);
    solrQuery.setFacet(true);
    solrQuery.addFacetPivotField(VENDOR_CODE_STATE);
    return solrQuery;
  }

  public static SolrInputDocument getSolrInputDocumentForAutoApprovalAssigneeAndUpdateState(String productCode, WorkflowState state) {
    SolrInputDocument solrInputDocument = new SolrInputDocument();
    solrInputDocument.setField(VendorProductSolrFieldNames.PRODUCT_CODE, productCode);
    solrInputDocument.setField(VendorProductSolrFieldNames.UPDATED_DATE,
        Collections.singletonMap(SolrConstants.SET_CLAUSE, new Date()));
    solrInputDocument.setField(VendorProductSolrFieldNames.ASSIGNED_DATE,
        Collections.singletonMap(SolrConstants.SET_CLAUSE, new Date()));
    solrInputDocument.setField(VendorProductSolrFieldNames.APPROVER_ASSIGNEE,
        Collections.singletonMap(SolrConstants.SET_CLAUSE, SolrConstants.AUTO_APPROVED));
    solrInputDocument.setField(VendorProductSolrFieldNames.APPROVED_DATE,
        Collections.singletonMap(SolrConstants.SET_CLAUSE, new Date()));
    solrInputDocument.setField(VendorProductSolrFieldNames.STATE,
        Collections.singletonMap(SolrConstants.SET_CLAUSE, state.toString()));
    return solrInputDocument;
  }

  private static boolean validateAndGetPostLiveStatus(Product product) {
    return product.isPostLive() && !product.isForceReview();
  }

  private static String buildFaultyImageTypeFilterQuery(String faultyImageType) {
    StringBuilder filterQueryBuilder = new StringBuilder();
    String imageViolationsField = VendorProductSolrFieldNames.IMAGE_VIOLATIONS;
    filterQueryBuilder.append(imageViolationsField).append(":");
    if (ProductLabels.GOOD.getDescription().equalsIgnoreCase(faultyImageType)) {
      filterQueryBuilder.append(SolrConstants.OPEN_BRACKET)
        .append(SolrConstants.NOT_APPLICABLE)
        .append(SolrConstants.CLOSE_BRACKET);
    } else {
      filterQueryBuilder.append(SolrConstants.OPEN_BRACKET)
        .append(SolrConstants.DOUBLE_QUOTES)
        .append(faultyImageType)
        .append(SolrConstants.DOUBLE_QUOTES)
        .append(SolrConstants.CLOSE_BRACKET);
    }
    return filterQueryBuilder.toString();
  }

  private static String buildBusinessPartnerCodeFilterQuery(String businessPartnerCode) {
    if (SolrConstants.EXTERNAL.equalsIgnoreCase(businessPartnerCode)) {
      return new StringBuilder().append(SolrConstants.NOT)
        .append(VendorProductSolrFieldNames.BUSINESS_PARTNER_CODE).append(SolrConstants.COLON)
        .append(SolrConstants.DOUBLE_QUOTES).append(SolrConstants.INTERNAL)
        .append(SolrConstants.DOUBLE_QUOTES).toString();
    } else if (SolrConstants.TRUSTED_SELLER.equalsIgnoreCase(businessPartnerCode)) {
      return new StringBuilder().append(VendorProductSolrFieldNames.SELLER_TYPE)
        .append(SolrConstants.COLON).append(SellerType.TRUSTED_SELLER.getValue()).toString();
    } else {
      return new StringBuilder().append(VendorProductSolrFieldNames.BUSINESS_PARTNER_CODE)
        .append(SolrConstants.COLON).append(SolrConstants.DOUBLE_QUOTES).append(businessPartnerCode)
        .append(SolrConstants.DOUBLE_QUOTES).toString();
    }
  }


  public static SolrQuery getSolrQueryForFilteredBoostedProducts(SummaryFilterDTO summaryFilterDTO,
    Pageable pageable, String storeId, SolrQuery boostQueryForAutoAssignment) {
    boostQueryForAutoAssignment.setStart(pageable.getPageNumber() * pageable.getPageSize());
    boostQueryForAutoAssignment.setRows(pageable.getPageSize());
    boostQueryForAutoAssignment.set(CommonParams.TZ, SolrConstants.JKT_TIME_ZONE);
    // post live filter
    if (Objects.nonNull(summaryFilterDTO.getPostLive())) {
      boostQueryForAutoAssignment.addFilterQuery(VendorProductSolrFieldNames.POST_LIVE + SolrConstants.COLON
        + summaryFilterDTO.getPostLive());
    } else {
      // include both post live and pre live products
      boostQueryForAutoAssignment.addFilterQuery(
        VendorProductSolrFieldNames.POST_LIVE + SolrConstants.COLON + SolrConstants.OPEN_BRACKET
          + String.join(SolrConstants.OR, String.valueOf(Boolean.TRUE),
          String.valueOf(Boolean.FALSE)) + SolrConstants.CLOSE_BRACKET);
    }
    //For product review Type
    String productReviewType = getProductReviewTypeForVendorList(summaryFilterDTO.getEdited(),
      summaryFilterDTO.getRevised());
    if (StringUtils.isNotEmpty(productReviewType)) {
      boostQueryForAutoAssignment.addFilterQuery(
        new StringBuilder().append(VendorProductSolrFieldNames.PRODUCT_REVIEW_TYPE)
          .append(SolrConstants.COLON).append(productReviewType).toString());
    }
    //For BrandPending
    if (Objects.nonNull(summaryFilterDTO.getBrandPending()) && Boolean.TRUE.equals(
      summaryFilterDTO.getBrandPending())) {
      boostQueryForAutoAssignment.addFilterQuery(
        VendorProductSolrFieldNames.BRAND_APPROVAL_STATUS + SolrConstants.COLON
          + SolrConstants.OPEN_BRACKET + String.join(SolrConstants.OR,
          BrandApprovalStatus.DRAFT.getValue(), BrandApprovalStatus.REJECTED.getValue())
          + SolrConstants.CLOSE_BRACKET);
    }
    boostQueryForAutoAssignment.addFilterQuery(
      VendorProductSolrFieldNames.MARK_FOR_DELETE + SolrConstants.COLON + false);
    if (Objects.nonNull(summaryFilterDTO.getContentPending()) || Objects.nonNull(
      summaryFilterDTO.getImagePending())) {
      boostQueryForAutoAssignment.addFilterQuery(
        VendorProductSolrFieldNames.REVIEW_TYPE + SolrConstants.COLON + getReviewTypeFromRequest(
          summaryFilterDTO.getContentPending(), summaryFilterDTO.getImagePending()));
    }
    boostQueryForAutoAssignment.addFilterQuery(VendorProductSolrFieldNames.STORE_ID + SolrConstants.COLON + storeId);
    //Time filters
    if(Objects.nonNull(summaryFilterDTO.getTimeFilterType())) {
      String timeFilterQuery = VendorProductSolrHelper.getQueryForProductCreatedDate(summaryFilterDTO.getTimeFilterType());
      if (!TimeFilterType.ALL.equals(summaryFilterDTO.getTimeFilterType()) && EnumUtils.isValidEnum(
        TimeFilterType.class, summaryFilterDTO.getTimeFilterType().name())) {
        boostQueryForAutoAssignment.addFilterQuery(
          VendorProductSolrFieldNames.PRODUCT_CREATED_DATE + SolrConstants.COLON + timeFilterQuery);
      }
    }
    if (Objects.nonNull(summaryFilterDTO.getRestrictedKeyword())) {
      boostQueryForAutoAssignment.addFilterQuery(
        VendorProductSolrFieldNames.RESTRICTED_KEYWORDS_PRESENT + SolrConstants.COLON
          + summaryFilterDTO.getRestrictedKeyword());
    }
    if (StringUtils.isNotEmpty(summaryFilterDTO.getFaultyImageType())) {
      String filterQuery = buildFaultyImageTypeFilterQuery(summaryFilterDTO.getFaultyImageType());
      boostQueryForAutoAssignment.addFilterQuery(filterQuery);
    }
    if (StringUtils.isNotBlank(summaryFilterDTO.getBusinessPartnerCode())) {
      String filterQuery = buildBusinessPartnerCodeFilterQuery(summaryFilterDTO.getBusinessPartnerCode());
      boostQueryForAutoAssignment.addFilterQuery(filterQuery);
    }
    boostQueryForAutoAssignment.set("indent", "true");
    boostQueryForAutoAssignment.addSort("score", SolrQuery.ORDER.desc);
    boostQueryForAutoAssignment.addSort(VendorProductSolrFieldNames.PRODUCT_CREATED_DATE,
      SolrQuery.ORDER.desc);
    return boostQueryForAutoAssignment;
  }

  private static void populateC1CategoryInfo(Product product, VendorProductSolr vendorProductSolr) {
    product.setC1CategoryCode(vendorProductSolr.getCategoryCodes().getLast());
    product.setC1CategoryName(vendorProductSolr.getCategoryNames().getLast());
  }
}
