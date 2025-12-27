package com.gdn.x.productcategorybase.repository.brand;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;

import com.gdn.x.productcategorybase.entity.brand.BrandWipHistory;

public interface BrandWipHistoryRepository extends JpaRepository<BrandWipHistory, String> {

  /**
   * Returns brand wip history according to brand code
   * @param brandCode
   * @param pageable
   * @return
   */
  Page<BrandWipHistory> findByBrandCodeOrderByCreatedDateDesc(String brandCode, Pageable pageable);

  /**
   * Returns brand wip history according to brand request code
   * @param brandRequestCode
   * @param pageable
   * @return
   */
  Page<BrandWipHistory> findByBrandRequestCodeOrderByCreatedDateDesc(String brandRequestCode, Pageable pageable);
}
