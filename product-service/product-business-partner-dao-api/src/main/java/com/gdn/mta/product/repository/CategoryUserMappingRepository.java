package com.gdn.mta.product.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import com.gdn.mta.product.entity.CategoryUserMapping;

/**
 * Created by hardikbohra on 12/05/18.
 */
public interface CategoryUserMappingRepository extends JpaRepository<CategoryUserMapping, String> {
}
