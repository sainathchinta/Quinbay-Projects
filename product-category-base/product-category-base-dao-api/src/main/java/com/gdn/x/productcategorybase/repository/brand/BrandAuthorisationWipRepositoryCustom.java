package com.gdn.x.productcategorybase.repository.brand;

import com.gdn.x.productcategorybase.BrandAuthorizationWipStatus;
import com.gdn.x.productcategorybase.entity.brand.BrandAuthorisationWip;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

import java.util.List;

public interface BrandAuthorisationWipRepositoryCustom {

  Page<BrandAuthorisationWip> findBrandAuthorisationWipBySellerCodeAndBrandNameAndAuthorisationStatus(
      String storeId, String sellerCode, String brandName, String status, Pageable pageable);

  Page<BrandAuthorisationWip> findBrandAuthorisationWipForExternalListing(String storeId,
      String sellerCode, String brandName, String tabName, List<BrandAuthorizationWipStatus> status,
      Pageable pageable);
}