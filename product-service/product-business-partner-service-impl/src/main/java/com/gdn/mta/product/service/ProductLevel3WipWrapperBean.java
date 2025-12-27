package com.gdn.mta.product.service;

import com.gdn.mta.product.entity.ProductCollection;
import com.gdn.mta.product.entity.ProductItemWholesalePrice;
import com.gdn.mta.product.entity.ProductLevel3Logistics;
import com.gdn.mta.product.repository.BusinessPartnerRepository;
import com.gdn.mta.product.service.util.MapperUtil;
import com.gdn.partners.pbp.dto.productlevel3.ProductLevel3AttributeWipResponse;
import com.gdn.partners.pbp.dto.productlevel3.ProductLevel3CountResponse;
import com.gdn.partners.pbp.dto.productlevel3.ProductLevel3ItemWipResponse;
import com.gdn.partners.pbp.dto.productlevel3.ProductLevel3WipDetailResponse;
import com.gdn.partners.pbp.entity.productlevel3.CountProductLevel3Wip;
import com.gdn.partners.pbp.entity.productlevel3.ProductLevel3AttributeWip;
import com.gdn.partners.pbp.entity.productlevel3.ProductLevel3ItemWip;
import com.gdn.partners.pbp.entity.productlevel3.ProductLevel3Wip;
import com.gdn.partners.pbp.service.productlevel1.ProductLevel1CollectionService;
import com.gdn.partners.pbp.service.productlevel3.ProductItemWholesalePriceService;
import com.gdn.partners.pbp.service.productlevel3.ProductLevel3LogisticsService;
import com.gdn.partners.pbp.service.productlevel3.ProductLevel3WipService;
import com.gdn.partners.product.orchestrator.constant.ProductLevel1State;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections.CollectionUtils;
import com.gdn.mta.product.util.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.function.Function;
import java.util.stream.Collectors;

@Service
@Slf4j
public class ProductLevel3WipWrapperBean implements ProductLevel3WipWrapper {

  @Autowired
  private ProductLevel3WipService productLevel3WipService;

  @Autowired
  private ProductLevel1CollectionService productLevel1CollectionService;

  @Autowired
  private ProductItemWholesalePriceService productItemWholesalePriceService;

  @Autowired
  private MapperUtil mapperUtil;

  @Autowired
  private BusinessPartnerRepository businessPartnerRepository;

  @Autowired
  private ProductLevel3LogisticsService productLevel3LogisticsService;

  @Value("${fetch.product.level3.count.cache.enabled}")
  private boolean fetchProductLevel3CountFromCacheEnabled;

  @Override
  public ProductLevel3WipDetailResponse findByProductSku(String storeId, String productSku, boolean isActive)
      throws Exception {
    ProductLevel3Wip productLevel3Wip = this.productLevel3WipService.findByProductSku(productSku, isActive);
    ProductCollection productCollection =
        this.productLevel1CollectionService.findByProductId(productLevel3Wip.getProductLevel1Id());
    List<String> itemSkus =
        productLevel3Wip.getItems().stream().map(productLevel3ItemWip -> productLevel3ItemWip.getGdnSku())
            .collect(Collectors.toList());
    List<ProductItemWholesalePrice> productItemWholesalePrices = new ArrayList<>();
    if (!isActive) {
      productItemWholesalePrices = productItemWholesalePriceService.findByStoreIdAndItemSkus(storeId, itemSkus);
    }
    ProductLevel3WipDetailResponse detail =
        this.generateProductLevel3WipDetailResponse(productLevel3Wip, productCollection, productItemWholesalePrices);
    // productCollection will be empty for deleted products
    if (Objects.isNull(productCollection) || ProductLevel1State.NEED_CORRECTION
        .equals(productCollection.getState())) {
      detail.setNotes(
          productLevel1CollectionService.findTop1ProductHistoryNotes(storeId, productLevel3Wip.getProductLevel1Id()));
    }
    return detail;
  }

  @Override
  public ProductLevel3CountResponse countSummaryByFilterType(String businessPartnerCode,
    String storeId, String type) throws Exception {
    if (fetchProductLevel3CountFromCacheEnabled) {
      return productLevel3WipService.countSummaryByFilterTypeCached(businessPartnerCode, storeId,
        type);
    }
    return productLevel3WipService.countSummaryByFilterType(businessPartnerCode, storeId, type);
  }

  @Override
  public CountProductLevel3Wip countSummaryWithState(String businessPartnerCode) throws Exception {
    if(fetchProductLevel3CountFromCacheEnabled){
      return productLevel3WipService.countSummaryWithStateCached(businessPartnerCode);
    }
    return productLevel3WipService.countSummaryWithState(businessPartnerCode);
  }

  private List<ProductLevel3Logistics> setProductL3LogisticOption(ProductLevel3Wip productLevel3Wip)
      throws Exception {
    ProfileResponse profileResponse = this.businessPartnerRepository
        .filterDetailByBusinessPartnerCode(productLevel3Wip.getBusinessPartnerCode());
    return this.productLevel3LogisticsService
        .findLogisticsByItemSku(productLevel3Wip.getItems().get(0).getGdnSku(),
            productLevel3Wip.getBusinessPartnerCode(),
            profileResponse.getCompany().getMerchantDeliveryType());
  }

  private ProductLevel3WipDetailResponse generateProductLevel3WipDetailResponse(ProductLevel3Wip productLevel3Wip,
      ProductCollection productCollection, List<ProductItemWholesalePrice> productItemWholesalePrices)
      throws Exception {
    ProductLevel3WipDetailResponse productLevel3WipDetailResponse = new ProductLevel3WipDetailResponse();
    BeanUtils.copyProperties(productLevel3Wip, productLevel3WipDetailResponse, "items", "attributes");
    Map<String, ProductItemWholesalePrice> itemSkuToWholesalePriceMap = new HashMap<>();
    if (CollectionUtils.isNotEmpty(productItemWholesalePrices)) {
      itemSkuToWholesalePriceMap = productItemWholesalePrices.stream().collect(
          Collectors.toMap(productItemWholesalePrice -> productItemWholesalePrice.getItemSku(), Function.identity()));
    }
    if (Objects.nonNull(productCollection)) {
      productLevel3WipDetailResponse.setBusinessPartnerName(productCollection.getBusinessPartnerName());
      productLevel3WipDetailResponse.setCategoryCode(productCollection.getCategoryCode());
      productLevel3WipDetailResponse.setProductCode(productCollection.getProductCode());
      productLevel3WipDetailResponse.setState(productCollection.getState());
    }
    for (ProductLevel3AttributeWip attribute : productLevel3Wip.getAttributes()) {
      ProductLevel3AttributeWipResponse attributeResponse = new ProductLevel3AttributeWipResponse();
      BeanUtils.copyProperties(attribute, attributeResponse);
      productLevel3WipDetailResponse.getAttributes().add(attributeResponse);
    }
    for (ProductLevel3ItemWip item : productLevel3Wip.getItems()) {
      ProductLevel3ItemWipResponse itemResonse = new ProductLevel3ItemWipResponse();
      BeanUtils.copyProperties(item, itemResonse, "wholesalePriceResponses", "wholesalePriceActivated");
      try {
        if (itemSkuToWholesalePriceMap.containsKey(item.getGdnSku())) {
          itemResonse.setProductItemWholesalePriceResponses(
              mapperUtil.mapStringToResponse(itemSkuToWholesalePriceMap.get(item.getGdnSku()).getWholesaleRules()));
        }
      } catch (IOException e) {
        log.error("Exception caught while mapping the wholesaleprice response itemsku :{}", item.getGdnSku(), e);
      }
      itemResonse.setWholesalePriceActivated((Objects.nonNull(itemSkuToWholesalePriceMap.get(item.getGdnSku())) ?
          itemSkuToWholesalePriceMap.get(item.getGdnSku()).isWholesalePriceActivated() :
          null));
      productLevel3WipDetailResponse.getItems().add(itemResonse);
    }
    productLevel3WipDetailResponse
        .setProductLevel3Logistics(setProductL3LogisticOption(productLevel3Wip));
    return productLevel3WipDetailResponse;
  }
}
