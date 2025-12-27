package com.gdn.partners.product.analytics.service.impl;

import java.util.Objects;

import com.gdn.partners.product.analytics.model.enums.ErrorCode;
import com.gdn.partners.product.analytics.service.impl.helper.ResponseHelper;
import com.gdn.partners.product.analytics.service.impl.util.ValidationUtil;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.partners.product.analytics.entity.AutoApprovedProductsUserFeedback;
import com.gdn.partners.product.analytics.repository.UserFeedbackRepository;
import com.gdn.partners.product.analytics.service.UserFeedbackService;
import com.gdn.partners.product.analytics.web.model.UserFeedbackResponse;
import com.gdn.partners.product.analytics.web.model.request.UserFeedbackRequest;
import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
public class UserFeedbackServiceImpl implements UserFeedbackService {

  @Autowired
  private UserFeedbackRepository userFeedbackRepository;

  @Autowired
  private ObjectMapper objectMapper;

  @Override
  public void updateUserFeedbackForAutoApprovedProduct(String productCode, UserFeedbackRequest request)
      throws Exception {
    ValidationUtil.checkParameter(StringUtils.isNotBlank(productCode),
        ErrorCode.PRODUCT_CODE_MUST_NOT_BE_BLANK.getMessage());
    AutoApprovedProductsUserFeedback feedbackFromDB = userFeedbackRepository.findByProductCode(productCode);
    //this is when feedback is being given for 1st time
    if (Objects.isNull(feedbackFromDB)) {
      feedbackFromDB = new AutoApprovedProductsUserFeedback();
      feedbackFromDB.setProductCode(productCode);
      feedbackFromDB.setUserFeedback(objectMapper.writeValueAsString(request));
    } else {
      UserFeedbackRequest existingUserFeedback;
      if (StringUtils.isNotEmpty(feedbackFromDB.getUserFeedback())) {
        existingUserFeedback =
            objectMapper.readValue(feedbackFromDB.getUserFeedback(), UserFeedbackRequest.class);
      } else {
        existingUserFeedback = new UserFeedbackRequest();
      }
      if (CollectionUtils.isNotEmpty(request.getUserImageFeedback())) {
        existingUserFeedback.setUserImageFeedback(request.getUserImageFeedback());
      }
      if (Objects.nonNull(request.getOtherModelFeedbackRequest())) {
        existingUserFeedback.setOtherModelFeedbackRequest(request.getOtherModelFeedbackRequest());
      }
      feedbackFromDB.setUserFeedback(objectMapper.writeValueAsString(existingUserFeedback));
    }
    userFeedbackRepository.save(feedbackFromDB);
  }

  @Override
  public UserFeedbackResponse fetchUserFeedbackResponse(String productCode) throws Exception {
    ValidationUtil.checkParameter(StringUtils.isNotBlank(productCode), ErrorCode.PRODUCT_CODE_MUST_NOT_BE_BLANK);
    AutoApprovedProductsUserFeedback autoApprovedProductsUserFeedback =
        userFeedbackRepository.findByProductCode(productCode);
    return ResponseHelper.toUserFeedbackResponse(productCode, autoApprovedProductsUserFeedback);
  }
}
