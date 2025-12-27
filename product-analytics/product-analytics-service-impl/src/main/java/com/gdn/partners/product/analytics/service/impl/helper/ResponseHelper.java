package com.gdn.partners.product.analytics.service.impl.helper;

import java.util.Objects;
import java.util.stream.Collectors;

import com.gdn.common.base.GdnPreconditions;
import com.gdn.partners.product.analytics.entity.SellerAnalytics;
import com.gdn.partners.product.analytics.model.ErrorMessages;
import com.gdn.partners.product.analytics.web.model.SellerAnalyticsResponse;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.BeanUtils;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.partners.product.analytics.entity.AutoApprovedProductsUserFeedback;
import com.gdn.partners.product.analytics.entity.AutoQCDetail;
import com.gdn.partners.product.analytics.web.model.AutoQCDetailResponse;
import com.gdn.partners.product.analytics.web.model.OtherModelFeedbackResponse;
import com.gdn.partners.product.analytics.web.model.UserFeedbackResponse;
import com.gdn.partners.product.analytics.web.model.UserImageFeedbackResponse;
import com.gdn.partners.product.analytics.web.model.request.OtherModelFeedbackRequest;
import com.gdn.partners.product.analytics.web.model.request.UserFeedbackRequest;
import com.gdn.partners.product.analytics.web.model.request.UserImageFeedbackRequest;

@Slf4j
public class ResponseHelper {

  private static final ObjectMapper objectMapper = new ObjectMapper();

  public static AutoQCDetailResponse getAutoQCDetailResponse(AutoQCDetail autoQCDetail) {
    if (Objects.nonNull(autoQCDetail)) {
      AutoQCDetailResponse autoQCDetailResponse = new AutoQCDetailResponse();
      BeanUtils.copyProperties(autoQCDetail, autoQCDetailResponse);
      return autoQCDetailResponse;
    }
    return null;
  }

  public static UserFeedbackResponse toUserFeedbackResponse(String productCode,
      AutoApprovedProductsUserFeedback autoApprovedProductsUserFeedback) throws Exception {
    UserFeedbackRequest userFeedbackInDB = new UserFeedbackRequest();
    UserFeedbackResponse userFeedbackResponse = new UserFeedbackResponse();
    userFeedbackResponse.setProductCode(productCode);
    if (Objects.isNull(autoApprovedProductsUserFeedback)) {
      return userFeedbackResponse;
    }
    try {
      userFeedbackInDB = objectMapper.readValue(autoApprovedProductsUserFeedback.getUserFeedback(),
        UserFeedbackRequest.class);
    } catch (Exception ex) {
      log.info("Error in reading user feedback {} ", userFeedbackInDB, ex);
    }
    if (Objects.nonNull(userFeedbackInDB.getOtherModelFeedbackRequest())) {
      userFeedbackResponse.setOtherModelFeedback(
          toOtherModelFeedbackResponse(userFeedbackInDB.getOtherModelFeedbackRequest()));
    }
    userFeedbackResponse.setUserImageFeedback(
        userFeedbackInDB.getUserImageFeedback().stream().map(ResponseHelper::toUserImageFeedbackResponse)
            .collect(Collectors.toList()));
    return userFeedbackResponse;
  }

  public static UserImageFeedbackResponse toUserImageFeedbackResponse(
      UserImageFeedbackRequest userImageFeedbackRequest) {
    UserImageFeedbackResponse userFeedbackResponse = new UserImageFeedbackResponse();
    BeanUtils.copyProperties(userImageFeedbackRequest, userFeedbackResponse);
    userFeedbackResponse.getImagePrediction().removeIf(String::isEmpty);
    return userFeedbackResponse;
  }

  public static OtherModelFeedbackResponse toOtherModelFeedbackResponse(
      OtherModelFeedbackRequest otherModelFeedbackRequest) {
    OtherModelFeedbackResponse otherModelFeedbackResponse = new OtherModelFeedbackResponse();
    BeanUtils.copyProperties(otherModelFeedbackRequest, otherModelFeedbackResponse);
    otherModelFeedbackResponse.getContentFeedBack().removeIf(String::isEmpty);
    return otherModelFeedbackResponse;
  }

  public static SellerAnalyticsResponse toSellerAnalyticsResponse(SellerAnalytics sellerAnalytics) {
    GdnPreconditions.checkArgument(Objects.nonNull(sellerAnalytics),
        ErrorMessages.SELLER_ANALYTICS_DATA_NOT_FOUND);
      SellerAnalyticsResponse sellerAnalyticsResponse = new SellerAnalyticsResponse();
      BeanUtils.copyProperties(sellerAnalytics, sellerAnalyticsResponse);
      return sellerAnalyticsResponse;
    }
}
