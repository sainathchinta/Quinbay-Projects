package com.gdn.mta.product.repository;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;

import com.gdn.mta.product.entity.ProductSystemParameter;

public interface ProductSystemParameterRepository extends JpaRepository<ProductSystemParameter, String> {

  ProductSystemParameter findByStoreIdAndVariable(String storeId, String variable);

  List<ProductSystemParameter> findByStoreIdAndVariableIn(String storeId, List<String> variable);
  List<ProductSystemParameter> findByStoreIdAndShowOnUIIsTrue(String storeId);
}
