package com.gdn.x.productcategorybase.service.impl;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import com.gdn.common.util.BeanUtils;
import com.gdn.x.productcategorybase.entity.ProductItem;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.gdn.x.productcategorybase.entity.Attribute;
import com.gdn.x.productcategorybase.entity.ProductItemAttributeValue;
import com.gdn.x.productcategorybase.repository.ProductItemAttributeRepository;
import com.gdn.x.productcategorybase.service.AttributeService;
import com.gdn.x.productcategorybase.service.ProductItemAttributeValueService;
import com.google.common.collect.Iterables;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@Service
@Transactional(readOnly = true)
public class ProductItemAttributeValueServiceBean implements ProductItemAttributeValueService {

  @Autowired
  private ProductItemAttributeRepository productItemAttributeRepository;

  @Autowired
  private AttributeService attributeService;

  @Value("${product.item.attributes.partition.size}")
  private int productItemAttributesPartitionSize;

  @Override
  public List<ProductItemAttributeValue> getDetachedProductItemAttributeValuesByStoreIdAndProductItemIds(
      String storeId, List<String> productItemIds) {
    Iterable<List<String>> partitionedProductItemIds = Iterables.partition(productItemIds, productItemAttributesPartitionSize);
    List<ProductItemAttributeValue> productItemAttributeValues = new ArrayList<>();
    for (List<String> productItemIdsList : partitionedProductItemIds) {
      productItemAttributeValues.addAll(getProductItemAttributeValues(storeId, productItemIdsList));
    }
    Map<String, List<ProductItemAttributeValue>> attributeIdProductItemAttributeValuesMap = new HashMap<>();
    for (ProductItemAttributeValue productItemAttributeValue : productItemAttributeValues) {
      if(!attributeIdProductItemAttributeValuesMap.containsKey(productItemAttributeValue.getAttributeId())) {
        attributeIdProductItemAttributeValuesMap.put(productItemAttributeValue.getAttributeId(), new ArrayList<>());
      }
      attributeIdProductItemAttributeValuesMap.get(productItemAttributeValue.getAttributeId()).add(productItemAttributeValue);
    }
    List<Attribute> attributes = attributeService.getAttributeByStoreIdAndAttributeIdsAndMarkForDeleteFalse(
        storeId, new ArrayList<>(attributeIdProductItemAttributeValuesMap.keySet()));
    for (Attribute attribute : attributes) {
      attributeIdProductItemAttributeValuesMap.get(attribute.getId()).forEach(
          productItemAttributeValue -> productItemAttributeValue.setAttribute(attribute));
    }
    return productItemAttributeValues;
  }

  private List<ProductItemAttributeValue> getProductItemAttributeValues(String storeId, List<String> productItemIds) {
    List<ProductItemAttributeValue> productItemAttributeValues =
        productItemAttributeRepository.findByStoreIdAndProductItemIdIn(storeId, productItemIds);
    List<ProductItemAttributeValue> clonedProductItemAttributeValues = new ArrayList<>();
    for (ProductItemAttributeValue productItemAttributeValue : productItemAttributeValues) {
      ProductItemAttributeValue clonedProductItemAttributeValue = new ProductItemAttributeValue();
      BeanUtils.copyProperties(productItemAttributeValue, clonedProductItemAttributeValue, "productItem");
      clonedProductItemAttributeValues.add(clonedProductItemAttributeValue);
    }
    return clonedProductItemAttributeValues;
  }

  @Override
  public void deleteByProductItemAttributeIds(List<String> productItemAttributeIds) {
    productItemAttributeRepository.deleteAllById(productItemAttributeIds);
  }

  @Override
  public void updateOnlyValueForProductItemAttributeValuesByAttributeId(String storeId,
      String attributeId, String value, ProductItem productItem) {
    ProductItemAttributeValue productItemAttributeValue =
        productItemAttributeRepository.findByStoreIdAndProductItemIdAndAttributeId(storeId,
            productItem.getId(), attributeId);
    if (Objects.isNull(productItemAttributeValue)) {
      return;
    }
    productItemAttributeValue.setValue(value);
    productItemAttributeRepository.save(productItemAttributeValue);
  }

}
