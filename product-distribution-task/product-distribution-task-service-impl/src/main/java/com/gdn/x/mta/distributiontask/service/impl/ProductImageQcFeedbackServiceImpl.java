package com.gdn.x.mta.distributiontask.service.impl;

import java.io.IOException;
import java.util.Objects;

import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.base.GdnPreconditions;
import com.gdn.x.mta.distributiontask.util.GdnMandatoryRequestParameterUtil;
import com.gdn.x.mta.distributiontask.dao.api.ProductImageQcFeedbackRepository;
import com.gdn.x.mta.distributiontask.model.ProductImageQcFeedback;
import com.gdn.x.mta.distributiontask.response.ProductImageQcFeedbackResponse;
import com.gdn.x.mta.distributiontask.rest.model.request.ProductImageQcFeedbackRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.UserFeedbackRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.UserFeedbackRequestOld;
import com.gdn.x.mta.distributiontask.service.api.ProductImageQcFeedbackService;

@Service
@Transactional(readOnly = true)
public class ProductImageQcFeedbackServiceImpl implements ProductImageQcFeedbackService {

  @Autowired
  private ProductImageQcFeedbackRepository productImageQcFeedbackRepository;

  private static final String OTHER_MODEL_FEEDBACK =  "otherModelFeedBack";

  @Override
  @Transactional(readOnly = false)
  public void upsertImageQcFeedback(ProductImageQcFeedbackRequest productImageQcFeedbackRequest,
      boolean updateUserFeedback, boolean updateSystemFeedback) throws Exception {
    if (Objects.nonNull(productImageQcFeedbackRequest.getProductCode())) {
      ProductImageQcFeedback productImageQcFeedback = productImageQcFeedbackRepository
          .findByStoreIdAndProductCode(GdnMandatoryRequestParameterUtil.getStoreId(),
              productImageQcFeedbackRequest.getProductCode());
      if (Objects.isNull(productImageQcFeedback)) {
        productImageQcFeedback = new ProductImageQcFeedback();
        productImageQcFeedback.setProductCode(productImageQcFeedbackRequest.getProductCode());
      }
      if (updateSystemFeedback) {
        productImageQcFeedback.setSystemFeedback(productImageQcFeedbackRequest.getSystemFeedback());
      }
      if (updateUserFeedback) {
        ObjectMapper objectMapper = new ObjectMapper();
        UserFeedbackRequest userFeedbackRequestUpdate =
            objectMapper.readValue(productImageQcFeedbackRequest.getUserFeedback(), UserFeedbackRequest.class);
        GdnPreconditions.checkArgument((Objects.nonNull(userFeedbackRequestUpdate.getUserFeedback()) && Objects.isNull(
                userFeedbackRequestUpdate.getOtherModelFeedBack())) || (
                Objects.isNull(userFeedbackRequestUpdate.getUserFeedback()) && Objects.nonNull(
                    userFeedbackRequestUpdate.getOtherModelFeedBack())),
            PdtServiceImplErrorMessage.USER_FEEDBACK_AND_OTHER_MODEL_FEEDBACK_NULL_ERROR);
        if (Objects.nonNull(productImageQcFeedback.getUserFeedback())) {
          settingUserFeedBackRequest(productImageQcFeedback, objectMapper, userFeedbackRequestUpdate);
        }
        productImageQcFeedbackRequest.setUserFeedback(objectMapper.writeValueAsString(userFeedbackRequestUpdate));
        productImageQcFeedback.setUserFeedback(productImageQcFeedbackRequest.getUserFeedback());
      }
      productImageQcFeedback.setStoreId(GdnMandatoryRequestParameterUtil.getStoreId());
      productImageQcFeedback.setFeedbackUpdated(productImageQcFeedbackRequest.isFeedbackUpdated());
      productImageQcFeedbackRepository.saveAndFlush(productImageQcFeedback);
    }
  }

  private static void settingUserFeedBackRequest(ProductImageQcFeedback productImageQcFeedback,
      ObjectMapper objectMapper, UserFeedbackRequest userFeedbackRequestUpdate) throws IOException {
    if (productImageQcFeedback.getUserFeedback().contains(OTHER_MODEL_FEEDBACK)) {
      UserFeedbackRequest userFeedbackRequestExisting =
          objectMapper.readValue(productImageQcFeedback.getUserFeedback(), UserFeedbackRequest.class);
      if (Objects.nonNull(userFeedbackRequestUpdate.getOtherModelFeedBack())) {
        userFeedbackRequestUpdate.setUserFeedback(userFeedbackRequestExisting.getUserFeedback());
      } else {
        userFeedbackRequestUpdate.setOtherModelFeedBack(userFeedbackRequestExisting.getOtherModelFeedBack());
      }
    } else {
      UserFeedbackRequestOld userFeedbackRequestExisting =
          objectMapper.readValue(productImageQcFeedback.getUserFeedback(), UserFeedbackRequestOld.class);
      if (Objects.nonNull(userFeedbackRequestUpdate.getOtherModelFeedBack())) {
        userFeedbackRequestUpdate.setUserFeedback(userFeedbackRequestExisting.getUserFeedback());
      } else {
        userFeedbackRequestUpdate.setOtherModelFeedBack(null);
      }
    }
  }

  @Override
  public ProductImageQcFeedbackResponse findProductQcFeedbackResponseByProductCode(String storeId, String productCode) {
    ProductImageQcFeedbackResponse productImageQcFeedbackResponse = new ProductImageQcFeedbackResponse();
    ProductImageQcFeedback productImageQcFeedback =
        productImageQcFeedbackRepository.findByStoreIdAndProductCode(storeId, productCode);
    if (Objects.isNull(productImageQcFeedback)) {
      return null;
    }
    BeanUtils.copyProperties(productImageQcFeedback, productImageQcFeedbackResponse);
    return productImageQcFeedbackResponse;
  }
}