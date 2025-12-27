package com.gdn.mta.bulk.service;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import com.gdn.mta.bulk.service.util.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import com.gdn.mta.bulk.dto.RecatProductSummaryRequest;
import com.gdn.mta.bulk.dto.RecatProductSummaryResponse;
import com.gdn.mta.bulk.entity.ProductRecatStatus;
import com.gdn.mta.bulk.repository.ProductBusinessPartnerRepository;
import com.gdn.mta.bulk.repository.ProductRecatStatusCustomRepository;
import com.gdn.mta.bulk.repository.ProductRecatStatusRepository;
import com.gdn.partners.bulk.util.Constant;
import com.gdn.partners.bulk.util.RecatConstants;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@Service
@Transactional(readOnly = true)
public class ProductRecatStatusServiceBean implements ProductRecatStatusService {

  @Autowired
  private ProductRecatStatusRepository productRecatStatusRepository;

  @Autowired
  private ProductRecatStatusCustomRepository productRecatStatusCustomRepository;

  @Autowired
  private ProductBusinessPartnerRepository productBusinessPartnerRepository;


  @Override
  @Transactional(readOnly = false, propagation = Propagation.REQUIRES_NEW)
  public void saveProductRecatStatusList(List<ProductRecatStatus> productRecatStatusList) {
    productRecatStatusRepository.saveAll(productRecatStatusList);
  }

  @Override
  public Map<String, Integer> getProductCountByRecatRequestCode(String storeId, String recatRequestCode) {
    List<Object[]> productCounts =
        productRecatStatusRepository.countProductsByRecatRequestCode(storeId, recatRequestCode);
    Map<String, Integer> productCountMap = generateEmptyCountMapWithKeys();
    productCounts.stream().filter(productCount -> Objects.nonNull(productCountMap.get(String.valueOf(productCount[0]))))
        .forEach(productCount -> productCountMap
            .put(String.valueOf(productCount[0]), ((Number) productCount[1]).intValue()));
    return productCountMap;
  }

  private Map<String, Integer> generateEmptyCountMapWithKeys() {
    Map<String, Integer> productCountmap = new HashMap<>();
    productCountmap.put(RecatConstants.PENDING, 0);
    productCountmap.put(RecatConstants.PUBLISHED, 0);
    productCountmap.put(RecatConstants.FINISHED, 0);
    productCountmap.put(RecatConstants.FAILED, 0);
    return productCountmap;
  }

  @Override
  public List<ProductRecatStatus> findProductRecatStatusByStoreIdAndAndStatus(String storeId, String status,
      int batchSize) {
    return productRecatStatusRepository
        .getProductRecatStatusByStoreIdAndStatus(storeId, status, PageRequest.of(0, batchSize));
  }

  @Override
  public Page<RecatProductSummaryResponse> getRecatProductSummary(String storeId, String recatRequestCode,
      RecatProductSummaryRequest recatProductSummaryRequest, int page, int size) {
    Page<ProductRecatStatus> productRecatStatuses = productRecatStatusCustomRepository
        .findProductRecatStatusByStatusFilterAndKeyword(storeId, recatRequestCode,
            recatProductSummaryRequest.getStatus(), recatProductSummaryRequest.getKeyword(), PageRequest.of(page, size));
    if (CollectionUtils.isNotEmpty(productRecatStatuses.getContent())) {
      List<RecatProductSummaryResponse> recatProductSummaryResponses =
          toRecatProductSummaryResponse(productRecatStatuses.getContent());
      return new PageImpl<>(recatProductSummaryResponses, PageRequest.of(page, size),
          productRecatStatuses.getTotalElements());
    } else {
      return new PageImpl<>(new ArrayList<>(), PageRequest.of(page, size), 0);
    }
  }

  private List<RecatProductSummaryResponse> toRecatProductSummaryResponse(
      List<ProductRecatStatus> productRecatStatuses) {
    List<RecatProductSummaryResponse> productSummaryResponses = new ArrayList<>();
    for (ProductRecatStatus productRecatStatus : productRecatStatuses) {
      RecatProductSummaryResponse productSummaryResponse = new RecatProductSummaryResponse();
      BeanUtils.copyProperties(productRecatStatus, productSummaryResponse, "status");
      productSummaryResponse.setStatus(
          RecatConstants.PENDING.equals(productRecatStatus.getStatus()) || RecatConstants.PUBLISHED
              .equals(productRecatStatus.getStatus()) ? RecatConstants.PENDING : productRecatStatus.getStatus());
      if (RecatConstants.PENDING.equals(productRecatStatus.getStatus()) || RecatConstants.PUBLISHED
          .equals(productRecatStatus.getStatus())) {
        productSummaryResponse.setNotes(RecatConstants.IN_PROGRESS_NOTE);
      } else if (RecatConstants.FINISHED.equals(productRecatStatus.getStatus())) {
        productSummaryResponse.setNotes(RecatConstants.SUCCEED_NOTE);
      } else {
        productSummaryResponse.setNotes(productRecatStatus.getErrorMessage());
      }
      productSummaryResponses.add(productSummaryResponse);
    }
    return productSummaryResponses;
  }


  @Override
  public ProductRecatStatus findByIdAndStatus(String id, String status) {
    return productRecatStatusRepository.findByIdAndStatus(id, status);
  }

  @Override
  public String updateProductCategory(ProductRecatStatus productRecatStatus) {
    return productBusinessPartnerRepository.updateProductCategory(productRecatStatus.getProductCode(),
        productRecatStatus.getNewCategoryCode(), productRecatStatus.getCreatedBy());
  }

  @Override
  @Transactional(readOnly = false)
  public void validateResponseAndSave(ProductRecatStatus productRecatStatus, String errorMessage) {
    if(StringUtils.isBlank(errorMessage)) {
      productRecatStatus.setStatus(RecatConstants.FINISHED);
    } else if(Constant.SYSTEM_ERROR.equals(errorMessage)) {
      productRecatStatus.setStatus(RecatConstants.FAILED);
      productRecatStatus.setSystemError(true);
      productRecatStatus.setErrorMessage(Constant.SYSTEM_ERROR);
    } else if(StringUtils.isNotBlank(errorMessage)) {
      productRecatStatus.setStatus(RecatConstants.FAILED);
      productRecatStatus.setValidationError(true);
      productRecatStatus
          .setErrorMessage(errorMessage.replaceAll(RecatConstants.VALIDATION_ERROR_MESSAGE, StringUtils.EMPTY));
    }
    log.info("Recat done for product : {} status : {} errorMessage : {} ", productRecatStatus.getProductCode(),
        productRecatStatus.getStatus(), productRecatStatus.getErrorMessage());
    productRecatStatusRepository.save(productRecatStatus);
  }

  @Override
  public int findCountByStoreIdAndStatusAndRecatRequestCode(String storeId, String status,
      String recatRequestCode) {
    return productRecatStatusRepository
        .findCountByStoreIdAndStatusAndRecatRequestCode(storeId, status, recatRequestCode);
  }

  @Override
  public List<ProductRecatStatus> findByStoreIdAndStatusAndRecatRequestCode(String storeId, String status,
      String recatRequestCode) {
    return productRecatStatusRepository.findByStoreIdAndStatusAndRecatRequestCode(storeId, status, recatRequestCode);
  }

  @Override
  public Map<String, Integer> getStatusCountByStoreIdAndRecatRequestCount(String storeId, String recatRequestCode) {
    List<Object[]> statusCount =
        productRecatStatusRepository.findStatusCountByStoreIdAndRecatRequestCode(storeId, recatRequestCode);
    Map<String, Integer> statusCountMap = new HashMap<>();
    statusCountMap.putIfAbsent(RecatConstants.PENDING, ((Number) statusCount.get(0)[0]).intValue());
    statusCountMap.putIfAbsent(RecatConstants.FINISHED, ((Number) statusCount.get(0)[1]).intValue());
    statusCountMap.putIfAbsent(RecatConstants.FAILED, ((Number) statusCount.get(0)[2]).intValue());
    statusCountMap.putIfAbsent(RecatConstants.VALIDATION_ERROR, ((Number) statusCount.get(0)[3]).intValue());
    statusCountMap.putIfAbsent(RecatConstants.SYSTEM_ERROR, ((Number) statusCount.get(0)[4]).intValue());
    return statusCountMap;
  }
}
