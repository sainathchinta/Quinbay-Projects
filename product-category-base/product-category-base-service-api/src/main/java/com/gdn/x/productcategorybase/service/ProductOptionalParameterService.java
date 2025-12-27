package com.gdn.x.productcategorybase.service;

import java.util.List;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

import com.gdn.common.base.service.GdnBaseService;
import com.gdn.x.productcategorybase.entity.ProductOptionalParameter;

public interface ProductOptionalParameterService extends GdnBaseService<ProductOptionalParameter> {
  List<ProductOptionalParameter> findByNameLike(String storeId, String name) throws Exception;

  Page<ProductOptionalParameter> findByNameLike(String storeId, String name, Pageable pageable) throws Exception;

  List<ProductOptionalParameter> findByProductCodeLike(String storeId, String productCode) throws Exception;

  Page<ProductOptionalParameter> findByProductCodeLike(String storeId, String productCode, Pageable pageable)
      throws Exception;

  List<ProductOptionalParameter> findByProductCodeLikeAndUniqueFalse(String storeId, String productCode)
      throws Exception;

  Page<ProductOptionalParameter> findByProductCodeLikeAndUniqueFalse(String storeId, String productCode,
      Pageable pageable) throws Exception;

  List<ProductOptionalParameter> findByProductCodeLikeAndUniqueTrue(String storeId, String productCode)
      throws Exception;

  Page<ProductOptionalParameter> findByProductCodeLikeAndUniqueTrue(String storeId, String productCode,
      Pageable pageable) throws Exception;

  void markForDeleteProductOptionalParameter(String id) throws Exception;
}
