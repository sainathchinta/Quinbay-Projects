package com.gdn.partners.pcu.internal.service;

import org.springframework.web.multipart.MultipartFile;

import com.gdn.partners.pcu.internal.web.model.request.BrandDeleteWebRequest;
import com.gdn.partners.pcu.internal.web.model.response.BrandLogo;
import com.gdn.partners.pcu.internal.web.model.response.BrandWebResponse;
import com.gdn.x.productcategorybase.dto.brand.UpdateBrandRequest;

public interface BrandService {

  /**
   *
   * @param brandCode
   * @param brandDeleteWebRequest
   */
  void deleteBrand(String brandCode, BrandDeleteWebRequest brandDeleteWebRequest);

  /**
   * Get brand detail by brand code
   *
   * @param brandCode
   * @return
   */
  BrandWebResponse getBrandDetail(String brandCode);

  /**
   * Returns brand logo by brand code
   *
   * @param brandCode
   * @return
   */
  BrandLogo filterBrandLogoByBrandCode(String brandCode, boolean isBrandLogo) throws Exception;

  /**
   * Returns brand logo by brand request code
   *
   * @param brandRequestCode
   * @return
   * @throws Exception
   */
  BrandLogo filterBrandLogoByBrandRequestCode(String brandRequestCode, boolean isBrandLogo) throws Exception;

  /**
   * @param request
   * @param brandLogo
   * @param profileBanner
   */
  void updateBrand(UpdateBrandRequest request, MultipartFile brandLogo, MultipartFile profileBanner) throws Exception;
}
