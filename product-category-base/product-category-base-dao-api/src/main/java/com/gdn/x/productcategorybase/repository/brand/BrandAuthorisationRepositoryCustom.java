package com.gdn.x.productcategorybase.repository.brand;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

import com.gdn.x.productcategorybase.entity.brand.BrandAuthorisation;

import java.util.List;

public interface BrandAuthorisationRepositoryCustom {

  Page<BrandAuthorisation> findBrandAuthorisationBySellerCodeAndBrandNameAndAuthorisationStatus(String storeId, String sellerCode, String brandName,
      String status, Pageable pageable, int brandAuthNearExpiryDaysThreshold);

  List<BrandAuthorisation> findSellerCodesByAuthorisationStatusAndConfiguration(String storeId,
      String status, int configDays);
}
