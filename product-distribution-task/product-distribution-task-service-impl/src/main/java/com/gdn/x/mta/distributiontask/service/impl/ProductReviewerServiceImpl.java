package com.gdn.x.mta.distributiontask.service.impl;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.function.Function;
import java.util.stream.Collectors;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.gdn.x.mta.distributiontask.dao.api.ProductReviewerRepository;
import com.gdn.x.mta.distributiontask.model.ProductReviewer;
import com.gdn.x.mta.distributiontask.service.api.ProductReviewerService;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@Service
@Transactional(readOnly = true)
public class ProductReviewerServiceImpl implements ProductReviewerService {

  @Autowired
  ProductReviewerRepository productReviewerRepository;

  @Override
  public ProductReviewer findProductReviewerByStoreIdAndProductCodeMarkForDeleteFalse(String storeId, String productCode) {
    return productReviewerRepository.findByStoreIdAndProductCode(storeId, productCode);
  }

  @Override
  public ProductReviewer findProductReviewerByProductCode(String productCode) {
    return productReviewerRepository.findByProductCode(productCode);
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public void save(ProductReviewer productReviewer) {
    productReviewerRepository.save(productReviewer);
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public void saveAndFlush(ProductReviewer productReviewer) {
    productReviewerRepository.saveAndFlush(productReviewer);
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public ProductReviewer addNewProduct(String storeId, String productCode) {
    ProductReviewer productReviewer = new ProductReviewer();
    productReviewer.setStoreId(storeId);
    productReviewer.setProductCode(productCode);
    productReviewer.setAssignedDate(null);
    productReviewer.setApproverAssignee(null);
    productReviewer.setApprovedDate(null);
    return productReviewerRepository.save(productReviewer);
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public ProductReviewer resetAssignmentData(ProductReviewer productReviewer, boolean clearAssignee) {
    productReviewer.setMarkForDelete(false);
    productReviewer.setAssignedDate(new Date());
    productReviewer.setApprovedDate(null);
    if (clearAssignee) {
      productReviewer.setApproverAssignee(null);
      productReviewer.setAssignedDate(null);
    }
    return productReviewerRepository.save(productReviewer);
  }

  @Override
  public List<ProductReviewer> findProductReviewerByProductCodesAndMarkForDeleteFalse(
    String storeId, List<String> productCodes) {
    return productReviewerRepository.findByStoreIdAndProductCodeIn(storeId, productCodes);
  }

  @Override
  public ProductReviewer findProductReviewerByStoreIdAndProductCode(String storeId, String productCode) {
    return productReviewerRepository.findByStoreIdAndProductCode(storeId, productCode);
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public void clearExistingReviewDatesDetails(String productCode) {
    ProductReviewer productReviewer = productReviewerRepository.findByProductCode(productCode);
    productReviewer.setApprovedDate(null);
    productReviewer.setAssignedDate(new Date());
    productReviewer.setMarkForDelete(false);
    productReviewerRepository.save(productReviewer);
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public void markForDeleteByProductCode(String productCode, String approverAssignee) {
    ProductReviewer productReviewer = productReviewerRepository.findByProductCode(productCode);
    if (Objects.nonNull(productReviewer)) {
      if (StringUtils.isNotEmpty(approverAssignee)) {
        productReviewer.setApproverAssignee(approverAssignee);
        productReviewer.setAssignedDate(new Date());
        productReviewer.setApprovedDate(new Date());
      }
      productReviewer.setMarkForDelete(true);
      productReviewerRepository.save(productReviewer);
    }
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public void clearAllReviewerDetails(String productCode) {
    ProductReviewer productReviewer =
        productReviewerRepository.findByProductCode(productCode);
    if (Objects.nonNull(productReviewer)) {
      productReviewer.setApproverAssignee(null);
      productReviewer.setAssignedDate(null);
      productReviewer.setApprovedDate(null);
      productReviewerRepository.save(productReviewer);
    }
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public void updateProductAssignment(String assignedTo, Date date, List<String> productCodes) {
    productReviewerRepository.updateProductAssignment(assignedTo, date, productCodes);
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public void deleteByProductCodesIn(List<String> productCodeList) {
    productReviewerRepository.deleteByProductCodeIn(productCodeList);
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public List<ProductReviewer> saveAll(List<ProductReviewer> productReviewers) {
    return productReviewerRepository.saveAll(productReviewers);
  }

  @Override
  public List<ProductReviewer> findProductReviewerByProductCodes(String storeId,
    List<String> productCodes) {
    if (CollectionUtils.isEmpty(productCodes)) {
      return new ArrayList<>();
    }
    return this.productReviewerRepository.findByStoreIdAndProductCodeIn(storeId, productCodes);
  }

  @Override
  public Map<String, ProductReviewer> findProductReviewerMapByProductCodes(String storeId, List<String> productCodes) {
    if (CollectionUtils.isEmpty(productCodes)) {
      return new HashMap<>();
    }
    return Optional.ofNullable(this.productReviewerRepository.findByStoreIdAndProductCodeIn(storeId, productCodes))
        .orElse(new ArrayList<>()).stream().collect(Collectors.toMap(ProductReviewer::getProductCode, Function.identity()));
  }

}
