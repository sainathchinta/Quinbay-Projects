package com.gdn.x.productcategorybase.service.impl;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import com.gdn.common.util.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.gdn.x.productcategorybase.entity.AllowedAttributeValue;
import com.gdn.x.productcategorybase.entity.ProductAttributeValue;
import com.gdn.x.productcategorybase.repository.ProductAttributeValueRepository;
import com.gdn.x.productcategorybase.service.ProductAttributeValueService;

import lombok.extern.slf4j.Slf4j;

@Slf4j
@Service
@Transactional(readOnly = true)
public class ProductAttributeValueServiceBean implements ProductAttributeValueService {

  @Autowired
  private ProductAttributeValueRepository repository;

  @Override
  public void delete(String id) throws Exception {
    // do nothing
  }

  @Override
  public ProductAttributeValue findById(String id) throws Exception {
    return this.repository.findById(id).orElse(null);
  }

  @Override
  public Page<ProductAttributeValue> findByStoreId(String storeId, Pageable pageable) throws Exception {
    return this.repository.findByStoreIdAndMarkForDeleteFalse(storeId, pageable);
  }

  @Override
  public List<ProductAttributeValue> findByStoreIdAndAllowedAttributeValue(String storeId,
      AllowedAttributeValue allowedAttributeValue) {
    return this.repository.findByStoreIdAndAllowedAttributeValue(storeId, allowedAttributeValue);
  }

  @Override
  public ProductAttributeValue findByStoreIdAndId(String storeId, String id) throws Exception {
    return this.repository.findByStoreIdAndIdAndMarkForDeleteFalse(storeId, id);
  }

  @Override
  @Transactional(readOnly = false)
  public String save(ProductAttributeValue entity) throws Exception {
    return ServiceBeanHelper.saveEntity(entity, this.repository);
  }

  @Override
  @Transactional(readOnly = false)
  public void update(ProductAttributeValue entity) throws Exception {
    ServiceBeanHelper.updateEntity(entity, this.repository);
  }

  @Override
  @Transactional(readOnly = true)
  public List<ProductAttributeValue> getProductAttributeValuesByStoreIdAndProductAttributeIdsAndMarkForDeleteFalseCached(
      String storeId, Set<String> productAttributeIds) {
    List<ProductAttributeValue> productAttributeValues =
        repository.findByStoreIdAndProductAttributeIdInAndMarkForDeleteFalse(storeId, productAttributeIds);
    List<ProductAttributeValue> clonedProductAttributeValues = new ArrayList<>();
    for (ProductAttributeValue productAttributeValue : productAttributeValues) {
      ProductAttributeValue clonedProductAttributeValue = new ProductAttributeValue();
      BeanUtils.copyProperties(productAttributeValue, clonedProductAttributeValue,
          "productAttribute", "allowedAttributeValue", "predefinedAllowedAttributeValue");
      clonedProductAttributeValues.add(clonedProductAttributeValue);
    }
    return clonedProductAttributeValues;
  }

  @Override
  @Transactional(readOnly = true)
  public List<ProductAttributeValue> getProductAttributeValuesByStoreIdAndProductAttributeIds(String storeId,
      Set<String> productAttributeIds) {
    List<ProductAttributeValue> productAttributeValues =
        repository.findByStoreIdAndProductAttributeIdIn(storeId, productAttributeIds);
    List<ProductAttributeValue> clonedProductAttributeValues = new ArrayList<>();
    for (ProductAttributeValue productAttributeValue : productAttributeValues) {
      ProductAttributeValue clonedProductAttributeValue = new ProductAttributeValue();
      BeanUtils.copyProperties(productAttributeValue, clonedProductAttributeValue, "productAttribute",
          "allowedAttributeValue", "predefinedAllowedAttributeValue");
      clonedProductAttributeValues.add(clonedProductAttributeValue);
    }
    return clonedProductAttributeValues;
  }

  @Override
  public List<ProductAttributeValue> saveProductAttributeValues(
      List<ProductAttributeValue> productAttributeValues) {
    return repository.saveAll(productAttributeValues);
  }
}
