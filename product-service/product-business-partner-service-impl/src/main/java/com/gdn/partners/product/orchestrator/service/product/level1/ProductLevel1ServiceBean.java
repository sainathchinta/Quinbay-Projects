package com.gdn.partners.product.orchestrator.service.product.level1;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Collectors;

import com.gdn.mta.product.service.ProductSystemParameterService;
import com.gdn.partners.pbp.commons.constants.SystemParameterConstants;
import org.apache.commons.collections4.ListUtils;
import com.gdn.mta.product.util.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;
import org.springframework.util.StringUtils;

import com.gdn.mta.product.util.GdnMandatoryRequestParameterUtil;
import com.gdn.mta.product.entity.ProductHistory;
import com.gdn.mta.product.entity.ProductWorkflow;
import com.gdn.mta.product.repository.ProductBusinessPartnerCustomRepository;
import com.gdn.mta.product.repository.ProductHistoryRepository;
import com.gdn.mta.product.repository.ProductRepository;
import com.gdn.mta.product.repository.ProductWorkflowRepository;
import com.gdn.partners.pbp.model.vo.ProductLv1IdxLv3IdVO;
import com.gdn.partners.pbp.workflow.WorkflowProcessCode;
import com.gdn.partners.product.orchestrator.constant.ProductLevel1State;
import com.gdn.partners.product.orchestrator.dto.product.level1.ProductLevel1FilterResponse;
import com.gdn.partners.product.orchestrator.entity.product.level1.ProductLevel1;
import com.gdn.partners.product.orchestrator.model.product.level1.ProductLevel1Filter;
import com.gdn.partners.product.orchestrator.repository.data.product.level1.ProductLevel1Repository;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;

import lombok.extern.slf4j.Slf4j;

@Service
@Transactional(readOnly = true)
@Slf4j
public class ProductLevel1ServiceBean implements ProductLevel1Service {

  @Autowired
  private ProductLevel1Repository productLevel1Repository;

  @Autowired
  private ProductBusinessPartnerCustomRepository productBusinessPartnerCustomRepository;

  @Autowired
  private ProductRepository productLevel1Outbound;

  @Autowired
  private ProductWorkflowRepository productWorkflowRepository;

  @Autowired
  private ProductHistoryRepository productHistoryRepository;

  @Autowired
  private ProductSystemParameterService productSystemParameterService;

  @Override
  public Page<ProductLevel1FilterResponse> findByFilter(ProductLevel1Filter filter, Pageable pageable)
      throws Exception {
    String storeId = GdnMandatoryRequestParameterUtil.getStoreId();
    int productFetchBatchSize = Integer.parseInt(productSystemParameterService
      .findByStoreIdAndVariable(storeId, SystemParameterConstants.PRODUCT_FETCH_BATCH_SIZE).getValue());
    List<ProductLv1IdxLv3IdVO> productLv1IdxLv3IdVOS = new ArrayList<>();
    if (!StringUtils.isEmpty(filter.getMerchantSku())) {
      productLv1IdxLv3IdVOS = this.productBusinessPartnerCustomRepository.findProductLv1IdxLv3IdVO(
          filter.getStoreCode(), filter.getMerchantSku(), null);
      if (!CollectionUtils.isEmpty(productLv1IdxLv3IdVOS)) {
        filter.setProductIds(productLv1IdxLv3IdVOS.stream()
            .map(ProductLv1IdxLv3IdVO::getProductId)
            .collect(Collectors.toSet()));
      } else {
        return new PageImpl<>(Collections.emptyList(), pageable,
            productLv1IdxLv3IdVOS.size());
      }
    }
    Map<String, ProductLevel1> productLevel1Map = new HashMap<>();
    Page<ProductLevel1> productLevel1s = this.productLevel1Repository
        .findByStoreIdAndFilterAndMarkForDeleteFalse(storeId, filter, pageable);
    Set<String> productCodes = productLevel1s.getContent().stream()
        .map(productLevel1 -> updateProductLevel1MapAndGetProductCode(productLevel1Map, productLevel1))
        .collect(Collectors.toSet());
    List<ProductLevel1FilterResponse> productLevel1FilterResponses = new ArrayList<>();
    if (!CollectionUtils.isEmpty(productCodes)) {
      List<List<String>> productCodesList =
        ListUtils.partition(new ArrayList<>(productCodes), productFetchBatchSize);
      List<ProductDetailResponse> masterProducts = new ArrayList<>();
      List<ProductDetailResponse> masterProductLocalList;

      for (List<String> productCodeList : productCodesList) {
        masterProductLocalList = this.productLevel1Outbound
          .getAllProductDetailsByProductCodes(GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), productCodeList).getContent();
        masterProducts.addAll(masterProductLocalList);
      }
      if (CollectionUtils.isEmpty(productLv1IdxLv3IdVOS)) {
        productLv1IdxLv3IdVOS = this.productBusinessPartnerCustomRepository.findProductLv1IdxLv3IdVO(
            filter.getStoreCode(), null,
            productLevel1s.getContent()
                .stream()
                .map(ProductLevel1::getProductId)
                .collect(Collectors.toList()));
      }
      productLevel1FilterResponses = buildProductLevel1FilterResponses(masterProducts, productLevel1Map);
      appendProductSkuInfo(productLevel1FilterResponses, productLevel1s, productLv1IdxLv3IdVOS);
      appendStateAndNote(productLevel1FilterResponses, productLevel1s);
    }
    return new PageImpl<>(productLevel1FilterResponses, pageable,
        productLevel1s.getTotalElements());
  }
  
  private List<ProductLevel1FilterResponse> buildProductLevel1FilterResponses(
      List<ProductDetailResponse> masterProducts, Map<String, ProductLevel1> productLevel1Map) {
    return masterProducts.stream()
        .filter(productDetailResponse -> Objects.nonNull(productLevel1Map.get(productDetailResponse.getProductCode())))
        .map(productDetailResponse -> buildProductLevel1FilterResponse(productDetailResponse,
            productLevel1Map.get(productDetailResponse.getProductCode()))).collect(Collectors.toList());
  }

  private String updateProductLevel1MapAndGetProductCode(Map<String, ProductLevel1> productLevel1Map,
      ProductLevel1 productLevel1) {
    productLevel1Map.put(productLevel1.getCode(), productLevel1);
    return productLevel1.getCode();
  }

  private ProductLevel1FilterResponse buildProductLevel1FilterResponse(ProductDetailResponse productDetailResponse,
      ProductLevel1 productLevel1) {
    ProductLevel1FilterResponse response = new ProductLevel1FilterResponse();
    BeanUtils.copyProperties(productDetailResponse, response);
    response.setPostLive(productLevel1.isPostLive());
    return response;
  }
  
  private void appendProductSkuInfo(List<ProductLevel1FilterResponse> productLevel1FilterResponses, 
      Page<ProductLevel1> productLevel1s, List<ProductLv1IdxLv3IdVO> productLv1IdxLv3IdVOS) {
    Map<String, ProductLv1IdxLv3IdVO> productIdXSkuMap =
        productLv1IdxLv3IdVOS.stream().filter(Objects::nonNull).filter(p -> p.getProductId() != null)
            .filter(p -> p.getGdnProductSku() != null)
            .collect(Collectors.toMap(ProductLv1IdxLv3IdVO::getProductId, Function.identity()));
    Map<String, ProductLv1IdxLv3IdVO> productCodeXSkuMap =
        productLevel1s.getContent().stream().filter(Objects::nonNull).filter(p -> p.getCode() != null)
            .filter(p -> productIdXSkuMap.get(p.getProductId()) != null).collect(Collectors
            .toMap(ProductLevel1::getCode, productLevel1 -> productIdXSkuMap.get(productLevel1.getProductId())));
    for(ProductLevel1FilterResponse productLevel1FilterResponse : productLevel1FilterResponses) {
      ProductLv1IdxLv3IdVO productLv1IdxLv3IdVO = productCodeXSkuMap.get(productLevel1FilterResponse.getProductCode());
      if (Objects.isNull(productLv1IdxLv3IdVO)) {
        continue;
      }
      productLevel1FilterResponse.setProductSku(productLv1IdxLv3IdVO.getGdnProductSku());
      productLevel1FilterResponse
          .setPreOrder(Objects.isNull(productLv1IdxLv3IdVO.getPreOrder()) ? false : productLv1IdxLv3IdVO.getPreOrder());
    }
  }

  private void appendStateAndNote(List<ProductLevel1FilterResponse> productLevel1FilterResponses,
      Page<ProductLevel1> productLevel1s) {
    Map<String, String> productCodeXStateMap = new HashMap<>();
    Map<String, String> productCodeXNotesMap = new HashMap<>();
    Map<String, List<String>> productIdXRejectionNotesMap = new HashMap<>();
    Map<String, String> productIdXRevisionNotesMap = new HashMap<>();
    for (ProductLevel1 productLevel1 : productLevel1s) {
      productCodeXStateMap.put(productLevel1.getCode(), productLevel1.getState());
      productCodeXNotesMap.put(productLevel1.getCode(), productLevel1.getReviewerNotes());
      if (Objects.nonNull(productLevel1.getState())) {
        if (ProductLevel1State.DELETED.equals(productLevel1.getState())) {
          productIdXRejectionNotesMap.put(productLevel1.getProductId(),
              getRejectedNote(productLevel1.getStoreId(), productLevel1.getProductId()));
        } else {
          productIdXRejectionNotesMap.put(productLevel1.getProductId(), null);
        }
        if (ProductLevel1State.NEED_CORRECTION.equals(productLevel1.getState())) {
          ProductHistory productHistory = productHistoryRepository
              .findTop1ByStoreIdAndProductIdAndMarkForDeleteFalseAndDescriptionOrderByCreatedDateDesc(
                  productLevel1.getStoreId(), productLevel1.getProductId(),
                  WorkflowProcessCode.RETURN_FOR_CORRECTION.getDesc());
          if (Objects.nonNull(productHistory)) {
            productIdXRevisionNotesMap.put(productLevel1.getProductId(), productHistory.getNotes());
          }
        } else {
          productIdXRevisionNotesMap.put(productLevel1.getProductId(), null);
        }
      }
    }
    for (ProductLevel1FilterResponse productLevel1FilterResponse : productLevel1FilterResponses) {
      productLevel1FilterResponse.setState(productCodeXStateMap.get(productLevel1FilterResponse.getProductCode()));
      productLevel1FilterResponse
          .setReviewerNotes(productCodeXNotesMap.get(productLevel1FilterResponse.getProductCode()));
      if (productIdXRejectionNotesMap.containsKey(productLevel1FilterResponse.getId())) {
        productLevel1FilterResponse
            .setRejectionNotes(String.valueOf(productIdXRejectionNotesMap.get(productLevel1FilterResponse.getId())));
      } else {
        productLevel1FilterResponse.setRejectionNotes(null);
      }
      productLevel1FilterResponse.setRevisionNotes(productIdXRevisionNotesMap.get(productLevel1FilterResponse.getId()));
    }
  }

  private List<String> getRejectedNote(String storeId, String productId) {
    List<ProductWorkflow> productWorkflows =
        productWorkflowRepository.findByStoreIdAndProductIdAndMarkForDeleteFalse(storeId, productId);
    return Optional.ofNullable(productWorkflows).orElse(Collections.emptyList()).stream().map(ProductWorkflow::getNotes)
        .collect(Collectors.toList());
  }
}
