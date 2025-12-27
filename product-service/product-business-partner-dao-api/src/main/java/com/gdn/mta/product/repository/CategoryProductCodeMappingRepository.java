package com.gdn.mta.product.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import com.gdn.mta.product.entity.CategoryProductCodeMapping;

/**
 * Created by hardikbohra on 12/05/18.
 */
public interface CategoryProductCodeMappingRepository extends JpaRepository<CategoryProductCodeMapping, String> {
}
