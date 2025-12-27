package com.gdn.x.productcategorybase.service.impl;

import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationException;
import com.gdn.x.productcategorybase.entity.ProductOptionalParameter;
import com.gdn.x.productcategorybase.repository.ProductOptionalParameterRepository;
import com.gdn.x.productcategorybase.service.ProductOptionalParameterService;

@Service
@Transactional(readOnly = true)
public class ProductOptionalParameterServiceBean implements ProductOptionalParameterService {

  private static final Logger LOG = LoggerFactory.getLogger(ProductOptionalParameterServiceBean.class);

  @Autowired
  private ProductOptionalParameterRepository repository;

  @Override
  public void delete(String id) throws Exception {
    // do nothing
  }

  @Override
  public ProductOptionalParameter findById(String id) throws Exception {
    return this.repository.findById(id).orElse(null);
  }

  @Override
  public List<ProductOptionalParameter> findByNameLike(String storeId, String name) throws Exception {
    return this.repository.findByStoreIdAndNameStartingWithAndMarkForDeleteFalse(storeId, name);
  }

  @Override
  public Page<ProductOptionalParameter> findByNameLike(String storeId, String name, Pageable pageable)
      throws Exception {
    return this.repository.findByStoreIdAndNameStartingWithAndMarkForDeleteFalse(storeId, name, pageable);
  }

  @Override
  public List<ProductOptionalParameter> findByProductCodeLike(String storeId, String productCode) throws Exception {
    return this.repository.findByStoreIdAndProductCodeStartingWithAndMarkForDeleteFalse(storeId, productCode);
  }

  @Override
  public Page<ProductOptionalParameter> findByProductCodeLike(String storeId, String productCode, Pageable pageable)
      throws Exception {
    return this.repository.findByStoreIdAndProductCodeStartingWithAndMarkForDeleteFalse(storeId, productCode, pageable);
  }

  @Override
  public List<ProductOptionalParameter> findByProductCodeLikeAndUniqueFalse(String storeId, String productCode)
      throws Exception {
    return this.repository.findByStoreIdAndProductCodeStartingWithAndUniqueFalseAndMarkForDeleteFalse(storeId,
        productCode);
  }

  @Override
  public Page<ProductOptionalParameter> findByProductCodeLikeAndUniqueFalse(String storeId, String productCode,
      Pageable pageable) throws Exception {
    return this.repository.findByStoreIdAndProductCodeStartingWithAndUniqueFalseAndMarkForDeleteFalse(storeId,
        productCode, pageable);
  }

  @Override
  public List<ProductOptionalParameter> findByProductCodeLikeAndUniqueTrue(String storeId, String productCode)
      throws Exception {
    return this.repository.findByStoreIdAndProductCodeStartingWithAndUniqueTrueAndMarkForDeleteFalse(storeId,
        productCode);
  }

  @Override
  public Page<ProductOptionalParameter> findByProductCodeLikeAndUniqueTrue(String storeId, String productCode,
      Pageable pageable) throws Exception {
    return this.repository.findByStoreIdAndProductCodeStartingWithAndUniqueTrueAndMarkForDeleteFalse(storeId,
        productCode, pageable);
  }

  @Override
  public Page<ProductOptionalParameter> findByStoreId(String storeId, Pageable pageable) throws Exception {
    return this.repository.findByStoreIdAndMarkForDeleteFalse(storeId, pageable);
  }


  @Override
  @Transactional(readOnly = false)
  public void markForDeleteProductOptionalParameter(String id) throws Exception {
    ProductOptionalParameter productOptionalParameter = this.findById(id);
    if (productOptionalParameter == null) {
      throw new ApplicationException(ErrorCategory.DATA_NOT_FOUND, "for optional product parameter with id : " + id);
    }
    try {
      productOptionalParameter.setMarkForDelete(true);
      this.repository.save(productOptionalParameter);
      ProductOptionalParameterServiceBean.LOG.debug("set mark for delete true : " + productOptionalParameter);
    } catch (Exception e) {
      throw new ApplicationException(ErrorCategory.DATA_ACCESS, "Optional Product Parameter", e);
    }
  }

  @Override
  @Transactional(readOnly = false)
  public String save(ProductOptionalParameter entity) throws Exception {
    String id = ServiceBeanHelper.saveEntity(entity, this.repository);
    return id;
  }

  @Override
  @Transactional(readOnly = false)
  public void update(ProductOptionalParameter entity) throws Exception {
    ServiceBeanHelper.updateEntity(entity, this.repository);
  }

}
