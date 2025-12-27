package com.gdn.partners.product.analytics.service.impl;

import com.gdn.partners.product.analytics.entity.AutoApprovedProducts;
import com.gdn.partners.product.analytics.entity.AutoApprovedProductsUserFeedback;
import com.gdn.partners.product.analytics.model.enums.AutoApprovedActionsEnum;
import com.gdn.partners.product.analytics.repository.AutoApprovedRepository;
import com.gdn.partners.product.analytics.repository.UserFeedbackRepository;
import com.gdn.partners.product.analytics.service.impl.util.CommonUtil;
import com.gdn.partners.product.analytics.service.AutoApprovedService;
import com.gdn.partners.product.analytics.service.impl.util.ValidationUtil;
import com.gdn.partners.product.analytics.web.model.AutoApprovedListWebResponse;
import com.gdn.partners.product.analytics.web.model.ProductAssigneeChangeResponse;
import com.gdn.partners.product.analytics.web.model.request.AutoApprovedAssigneeRequest;
import com.gdn.partners.product.analytics.web.model.request.AutoApprovedSelectedDownloadRequest;
import com.gdn.partners.product.analytics.web.model.request.AutoApprovedWebRequest;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;

import com.gdn.partners.product.analytics.model.enums.ErrorCode;

@Service
@Slf4j
public class AutoApprovedServiceImpl implements AutoApprovedService {

  @Autowired
  private AutoApprovedRepository autoApprovedRepository;

  @Autowired
  private UserFeedbackRepository userFeedbackRepository;

  @Override
  public Page<AutoApprovedListWebResponse> fetchListOfAutoApprovedProducts(
    AutoApprovedWebRequest request, int page, int size) {
    boolean isProductCode = false;
    if (StringUtils.isNotBlank(request.getKeyword())) {
      isProductCode = ValidationUtil.isProductCode(request.getKeyword());
    }
    Page<AutoApprovedProducts> autoApprovedProductsList =
      autoApprovedRepository.fetchListOfAutoApprovedProducts(request, PageRequest.of(page, size),
        isProductCode);
    if (Objects.isNull(autoApprovedProductsList) || CollectionUtils.isEmpty(
      autoApprovedProductsList.getContent())) {
      return new PageImpl<>(new ArrayList<>(), PageRequest.of(page, size), 0);
    }
    List<AutoApprovedListWebResponse> autoApprovedProducts =
      autoApprovedProductsList.getContent().stream().map(CommonUtil::mapToAutoApprovedWebResponse)
        .collect(Collectors.toList());
    return new PageImpl<>(autoApprovedProducts, PageRequest.of(page, size),
      autoApprovedProductsList.getTotalElements());
  }

  @Override
  public List<AutoApprovedListWebResponse> fetchSelectedItemsOfAutoApprovedProducts(
    AutoApprovedSelectedDownloadRequest request) {
    ValidationUtil.checkParameter(CollectionUtils.isNotEmpty(request.getProductCodes()),
      ErrorCode.PRODUCT_CODE_LIST_EMPTY.getMessage());
    return request.getProductCodes().stream().distinct().map(
        productCode -> autoApprovedRepository.findByProductCodeAndMarkForDeleteFalse(productCode))
      .filter(Objects::nonNull).map(CommonUtil::mapToAutoApprovedWebResponse)
      .collect(Collectors.toList());
  }

  @Override
  public List<ProductAssigneeChangeResponse> updateAssignee(
    AutoApprovedAssigneeRequest approvedAssigneeRequest) {
    ValidationUtil.checkParameter(
      CollectionUtils.isNotEmpty(approvedAssigneeRequest.getProductCode()),
      ErrorCode.PRODUCT_CODE_LIST_EMPTY.getMessage());
    return approvedAssigneeRequest.getProductCode().stream().distinct().map(
        productCode -> updateAssigneeChange(productCode, approvedAssigneeRequest.getAssigneeTo()))
      .filter(Objects::nonNull).collect(Collectors.toList());
  }

  @Override
  public void deleteAutoApprovedProduct(String productCode, String action) {
    AutoApprovedProducts autoApprovedProducts =
        autoApprovedRepository.findByProductCodeAndMarkForDeleteFalse(productCode);
    if (!AutoApprovedActionsEnum.AUTO_HEAL.name().equals(action)) {
      AutoApprovedProductsUserFeedback feedbackResponse =
        userFeedbackRepository.findByProductCode(productCode);
      if (Objects.isNull(feedbackResponse)) {
        feedbackResponse = new AutoApprovedProductsUserFeedback();
        feedbackResponse.setProductCode(productCode);
      }
      feedbackResponse.setAction(action);
      userFeedbackRepository.save(feedbackResponse);
    }
    if(Objects.nonNull(autoApprovedProducts)) {
      autoApprovedProducts.setMarkForDelete(true);
      autoApprovedRepository.save(autoApprovedProducts);
    }
  }

  private ProductAssigneeChangeResponse updateAssigneeChange(String productCode,
    String assignedTo) {
    if (StringUtils.isBlank(productCode)) {
      return new ProductAssigneeChangeResponse(productCode,
        ErrorCode.PRODUCT_CODE_MUST_NOT_BE_BLANK.getMessage());
    }
    AutoApprovedProducts approvedProducts =
      autoApprovedRepository.findByProductCodeAndMarkForDeleteFalse(productCode);
    if (Objects.isNull(approvedProducts)) {
      return new ProductAssigneeChangeResponse(productCode,
        ErrorCode.PRODUCT_NOT_FOUND.getMessage());
    }
    if (StringUtils.isEmpty(assignedTo) && StringUtils.isEmpty(approvedProducts.getAssignedTo())) {
      return new ProductAssigneeChangeResponse(productCode,
        ErrorCode.PRODUCT_IS_ALREADY_UNASSIGNED.getMessage());
    }
    approvedProducts.setAssignedDate(new Date());
    approvedProducts.setAssignedTo(assignedTo);
    log.info("Updating assignee {} for product code {}", assignedTo, assignedTo);
    autoApprovedRepository.save(approvedProducts);
    return null;
  }
}
