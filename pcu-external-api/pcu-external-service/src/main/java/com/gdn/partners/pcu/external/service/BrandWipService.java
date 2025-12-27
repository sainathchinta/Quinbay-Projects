package com.gdn.partners.pcu.external.service;

import org.springframework.web.multipart.MultipartFile;

import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandResponse;
import com.gdn.x.productcategorybase.dto.brand.CreateBrandWipRequest;
import com.gdn.x.productcategorybase.dto.brand.CreateBrandWipResponse;

public interface BrandWipService {
  /**
   *
   * @param createBrandWipRequest
   * @param brandLogo
   * @param profileBanner
   * @return
   * @throws Exception
   */
  GdnRestSingleResponse<CreateBrandWipResponse> create(CreateBrandWipRequest createBrandWipRequest,
      MultipartFile brandLogo, MultipartFile profileBanner) throws Exception;

  /**
   * Search if a brand exists of given name
   * @param name
   * @return
   */
  BrandResponse findByName(String name);
}
