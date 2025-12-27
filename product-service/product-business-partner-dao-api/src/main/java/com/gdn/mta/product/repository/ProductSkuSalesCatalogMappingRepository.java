package com.gdn.mta.product.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import com.gdn.mta.product.entity.ProductSkuSalesCatalogMapping;

/**
 * Created by hardikbohra on 03/06/18.
 */
public interface ProductSkuSalesCatalogMappingRepository extends JpaRepository<ProductSkuSalesCatalogMapping, String> {
}
