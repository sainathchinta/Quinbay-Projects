package com.gdn.mta.product.util;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

import org.apache.solr.common.SolrDocument;
import org.apache.solr.common.SolrDocumentList;
import org.apache.solr.common.SolrInputDocument;

import com.gda.mta.product.dto.ProductBusinessPartnerMapperResponse;
import com.gda.mta.product.dto.response.AssigneeResponse;
import com.gda.mta.product.dto.response.ReviewProductResponse;
import com.gdn.mta.product.entity.UpdatedProductHistory;
import com.gdn.mta.product.entity.ProductCollection;
import com.gdn.mta.product.valueobject.HistorySolr;
import com.gdn.partners.pbp.commons.util.SolrFieldNames;

public class SummaryFilterUtil {

  public static List<ReviewProductResponse> toReviewProductResponseList(SolrDocumentList solrDocumentList) {
    return solrDocumentList.stream()
        .map(SummaryFilterUtil::toReviewProductResponse)
        .collect(Collectors.toList());
  }

  private static ReviewProductResponse toReviewProductResponse(SolrDocument solrDocument) {
    ReviewProductResponse reviewProductResponse =
        ReviewProductResponse.builder().productId(String.valueOf(solrDocument.getFieldValue(SolrFieldNames.PRODUCT_ID)))
            .productCode(String.valueOf(solrDocument.getFieldValue(SolrFieldNames.PRODUCT_CODE)))
            .productName(String.valueOf(solrDocument.getFieldValue(SolrFieldNames.PRODUCT_NAME)))
            .businessPartnerCode(String.valueOf(solrDocument.getFieldValue(SolrFieldNames.BUSINESS_PARTNER_CODE)))
            .businessPartnerName(String.valueOf(solrDocument.getFieldValue(SolrFieldNames.BUSINESS_PARTNER_NAME)))
            .brand(String.valueOf(solrDocument.getFieldValue(SolrFieldNames.BRAND)))
            .assignedTo(String.valueOf(solrDocument.getFieldValue(SolrFieldNames.ASSIGNED_TO)))
            .state(String.valueOf(solrDocument.getFieldValue(SolrFieldNames.STATE)))
            .submittedDate((Date) solrDocument.getFieldValue(SolrFieldNames.SUBMITTED_DATE))
            .categoryCode(((List<String>) solrDocument.getFieldValue(SolrFieldNames.CATEGORY_CODES)).get(0))
            .categoryName(((List<String>) solrDocument.getFieldValue(SolrFieldNames.CATEGORY_NAMES)).get(0))
            .isSourceDb(false).build();
    reviewProductResponse.setId(String.valueOf(solrDocument.getFieldValue(SolrFieldNames.ID)));
    reviewProductResponse.setCreatedBy(String.valueOf(solrDocument.getFieldValue(SolrFieldNames.CREATED_BY)));
    reviewProductResponse.setUpdatedDate((Date) solrDocument.getFieldValue(SolrFieldNames.UPDATED_DATE));
    reviewProductResponse.setCreatedDate((Date) solrDocument.getFieldValue(SolrFieldNames.CREATED_DATE));
    return reviewProductResponse;
  }

  public static List<ProductBusinessPartnerMapperResponse> convertQueryResponseToProductBusinessPartnerMapperResponseList(
      SolrDocumentList solrDocuments) {
    return solrDocuments.stream().map(SummaryFilterUtil::toProductBusinessPartnerMapperResponse)
        .collect(Collectors.toList());
  }

  public static List<ProductBusinessPartnerMapperResponse> convertProductCollectionToProductBusinessPartnerMapperResponseList(
      List<ProductCollection> businessPartners) {
    return businessPartners.stream().map(SummaryFilterUtil::convertProductCollectionToProductBusinessPartnerMapperResponse)
        .collect(Collectors.toList());
  }

  private static ProductBusinessPartnerMapperResponse convertProductCollectionToProductBusinessPartnerMapperResponse(ProductCollection productCollection) {
    ProductBusinessPartnerMapperResponse productBusinessPartnerMapperResponse = new ProductBusinessPartnerMapperResponse();
    productBusinessPartnerMapperResponse.setBusinessPartnerCode(productCollection.getBusinessPartnerCode());
    productBusinessPartnerMapperResponse.setBusinessPartnerName(productCollection.getBusinessPartnerName());
    return productBusinessPartnerMapperResponse;
  }

  private static ProductBusinessPartnerMapperResponse toProductBusinessPartnerMapperResponse(
      SolrDocument solrDocument) {
    ProductBusinessPartnerMapperResponse mapperResponse = new ProductBusinessPartnerMapperResponse();
    mapperResponse
        .setBusinessPartnerCode(String.valueOf(solrDocument.getFieldValue(SolrFieldNames.BUSINESS_PARTNER_CODE)));
    mapperResponse
        .setBusinessPartnerName(String.valueOf(solrDocument.getFieldValue(SolrFieldNames.BUSINESS_PARTNER_NAME)));
    return mapperResponse;
  }

  public static List<AssigneeResponse> toAssigneeResponseList(SolrDocumentList solrDocuments) {
    return solrDocuments.stream().map(SummaryFilterUtil::toAssigneeResponse).collect(Collectors.toList());
  }

  public static List<AssigneeResponse> convertAssigneeListToAssigneeResponses(List<String> assigneeList) {
    return assigneeList.stream().map(SummaryFilterUtil::covertStringToAssigneeResponse).collect(Collectors.toList());
  }

  private static AssigneeResponse covertStringToAssigneeResponse(String assignedto) {
    AssigneeResponse assigneeResponse = new AssigneeResponse();
    assigneeResponse.setAssignee(assignedto);
    return assigneeResponse;
  }

  private static AssigneeResponse toAssigneeResponse(SolrDocument solrDocument) {
    AssigneeResponse assigneeResponse = new AssigneeResponse();
    assigneeResponse.setAssignee(String.valueOf(solrDocument.getFieldValue(SolrFieldNames.ASSIGNED_TO)));
    return assigneeResponse;
  }

  public static List<ReviewProductResponse> convertProductCollectionListToReviewProductResponseList(
      List<ProductCollection> productCollectionList) {
    return productCollectionList.stream().map(SummaryFilterUtil::convertProductCollectionToReviewProductResponse)
        .collect(Collectors.toList());
  }

  private static ReviewProductResponse convertProductCollectionToReviewProductResponse(
      ProductCollection productCollection) {
    ReviewProductResponse reviewProductResponse =
        ReviewProductResponse.builder().productId(productCollection.getProductId())
            .productCode(productCollection.getProductCode()).productName(productCollection.getProductName())
            .businessPartnerCode(productCollection.getBusinessPartnerCode())
            .businessPartnerName(productCollection.getBusinessPartnerName()).brand(productCollection.getBrand())
            .assignedTo(productCollection.getAssignedTo()).state(productCollection.getState())
            .submittedDate(productCollection.getSubmittedDate()).categoryCode(productCollection.getCategoryCode())
            .categoryName(productCollection.getCategoryName()).isSourceDb(true).build();
    return reviewProductResponse;
  }

  public static List<HistorySolr> convertToHistorySolrList(SolrDocumentList solrDocuments) {
    if (solrDocuments.isEmpty()) {
      return new ArrayList<>();
    } else {
      return solrDocuments.stream().map(SummaryFilterUtil::convertToHistorySolr).collect(Collectors.toList());
    }
  }

  private static HistorySolr convertToHistorySolr(SolrDocument solrDocument) {
    HistorySolr historySolr = new HistorySolr();
    if (Objects.nonNull(solrDocument.getFieldValue(SolrFieldNames.ID))) {
      historySolr.setId(String.valueOf(solrDocument.getFieldValue(SolrFieldNames.ID)));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(SolrFieldNames.ACCESS_TIME))) {
      historySolr.setAccessTime((Date) solrDocument.getFieldValue(SolrFieldNames.ACCESS_TIME));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(SolrFieldNames.PRODUCT_SKU))) {
      historySolr.setProductSku(String.valueOf(solrDocument.getFieldValue(SolrFieldNames.PRODUCT_SKU)));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(SolrFieldNames.GDN_SKU))) {
      historySolr.setGdnSku(String.valueOf(solrDocument.getFieldValue(SolrFieldNames.GDN_SKU)));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(SolrFieldNames.GDN_NAME))) {
      historySolr.setGdnName(String.valueOf(solrDocument.getFieldValue(SolrFieldNames.GDN_NAME)));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(SolrFieldNames.ACTIVITY))) {
      historySolr.setActivity(String.valueOf(solrDocument.getFieldValue(SolrFieldNames.ACTIVITY)));
    }
    return historySolr;
  }

  public static List<SolrInputDocument> toSolrInputDocumentsForHistory(
      List<UpdatedProductHistory> updatedProductHistories) {
    return updatedProductHistories.stream().map(SummaryFilterUtil::toSolrInputDocument)
        .collect(Collectors.toList());
  }

  public static SolrInputDocument toSolrInputDocument(UpdatedProductHistory updatedProductHistory) {
    SolrInputDocument solrInputDocument = new SolrInputDocument();
    solrInputDocument.setField(SolrFieldNames.ID, updatedProductHistory.getAuditTrailId());
    if (Objects.nonNull(updatedProductHistory.getProductSku())) {
      solrInputDocument.setField(SolrFieldNames.PRODUCT_SKU, updatedProductHistory.getProductSku());
    }
    if (Objects.nonNull(updatedProductHistory.getGdnSku())) {
      solrInputDocument.setField(SolrFieldNames.GDN_SKU, updatedProductHistory.getGdnSku());
    }
    if (Objects.nonNull(updatedProductHistory.getActivity())) {
      solrInputDocument.setField(SolrFieldNames.ACTIVITY, updatedProductHistory.getActivity());
    }
    if (Objects.nonNull(updatedProductHistory.getAccessTime())) {
      solrInputDocument.setField(SolrFieldNames.ACCESS_TIME, updatedProductHistory.getAccessTime());
    }
    if (Objects.nonNull(updatedProductHistory.getGdnName())) {
      solrInputDocument.setField(SolrFieldNames.GDN_NAME, updatedProductHistory.getGdnName());
    }
    if (Objects.nonNull(updatedProductHistory.getGdnName())) {
      solrInputDocument.setField(SolrFieldNames.PICKUP_POINT_CODE, updatedProductHistory.getPickupPointCode());
    }
    if (Objects.nonNull(updatedProductHistory.getGdnName())) {
      solrInputDocument.setField(SolrFieldNames.ONLINE_STATUS, updatedProductHistory.getOnlineStatus());
    }
    return solrInputDocument;
  }
}
