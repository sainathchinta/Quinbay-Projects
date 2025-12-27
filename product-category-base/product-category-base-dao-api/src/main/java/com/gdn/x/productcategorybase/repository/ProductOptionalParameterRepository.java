package com.gdn.x.productcategorybase.repository;

import java.util.List;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;

import com.gdn.x.productcategorybase.entity.ProductOptionalParameter;

public interface ProductOptionalParameterRepository extends JpaRepository<ProductOptionalParameter, String> {
  List<ProductOptionalParameter> findByStoreIdAndMarkForDeleteFalse(String storeId);

  Page<ProductOptionalParameter> findByStoreIdAndMarkForDeleteFalse(String storeId, Pageable pageable);

  List<ProductOptionalParameter> findByStoreIdAndNameStartingWithAndMarkForDeleteFalse(String storeId, String name);

  Page<ProductOptionalParameter> findByStoreIdAndNameStartingWithAndMarkForDeleteFalse(String storeId, String name,
      Pageable pageable);

  List<ProductOptionalParameter> findByStoreIdAndProductCodeAndMarkForDeleteFalse(String storeId, String productCode);

  Page<ProductOptionalParameter> findByStoreIdAndProductCodeAndMarkForDeleteFalse(String storeId, String productCode,
      Pageable pageable);

  List<ProductOptionalParameter> findByStoreIdAndProductCodeStartingWithAndMarkForDeleteFalse(String storeId,
      String productCode);

  Page<ProductOptionalParameter> findByStoreIdAndProductCodeStartingWithAndMarkForDeleteFalse(String storeId,
      String productCode, Pageable pageable);

  List<ProductOptionalParameter> findByStoreIdAndProductCodeStartingWithAndUniqueFalseAndMarkForDeleteFalse(
      String storeId, String productCode);

  Page<ProductOptionalParameter> findByStoreIdAndProductCodeStartingWithAndUniqueFalseAndMarkForDeleteFalse(
      String storeId, String productCode, Pageable pageable);

  List<ProductOptionalParameter> findByStoreIdAndProductCodeStartingWithAndUniqueTrueAndMarkForDeleteFalse(
      String storeId, String productCode);

  Page<ProductOptionalParameter> findByStoreIdAndProductCodeStartingWithAndUniqueTrueAndMarkForDeleteFalse(
      String storeId, String productCode, Pageable pageable);
}
