package com.gdn.partners.pcu.internal.web.controller;

import static com.gdn.partners.pcu.internal.model.ErrorMessages.BRAND_CODE_AND_BRAND_REQUEST_CODE_EMPTY;
import static com.gdn.partners.pcu.internal.model.ErrorMessages.BRAND_CODE_AND_BRAND_REQUEST_CODE_SENT;


import jakarta.servlet.ServletOutputStream;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.validation.Valid;
import org.apache.commons.lang.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RequestPart;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.partners.core.exception.ApplicationException;
import com.gdn.partners.core.web.dto.BaseResponse;
import com.gdn.partners.core.web.dto.SingleBaseResponse;
import com.gdn.partners.pcu.internal.client.helper.ClientParameterHelper;
import com.gdn.partners.pcu.internal.model.BrandApiPath;
import com.gdn.partners.pcu.internal.service.BrandService;
import com.gdn.partners.pcu.internal.validaton.annotation.UpdateBrandRequestValid;
import com.gdn.partners.pcu.internal.service.impl.util.ConverterUtil;
import com.gdn.partners.pcu.internal.web.model.request.BrandDeleteWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.UpdateBrandWebRequest;
import com.gdn.partners.pcu.internal.web.model.response.BrandLogo;
import com.gdn.partners.pcu.internal.web.model.response.BrandWebResponse;
import com.gdn.x.productcategorybase.dto.brand.UpdateBrandRequest;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@Tag(name = "Brand Controller")
@RestController
@RequestMapping(value = BrandApiPath.BASE_PATH)
@Validated
public class BrandController {

  @Autowired
  private BrandService brandService;

  @Autowired
  private ClientParameterHelper clientParameterHelper;

  @Operation(summary = "Delete brand by brandCode")
  @DeleteMapping(value = BrandApiPath.DELETE, produces = MediaType.APPLICATION_JSON_VALUE,
      consumes = MediaType.APPLICATION_JSON_VALUE)
  public BaseResponse deleteBrand(
      @PathVariable String brandCode, @RequestBody BrandDeleteWebRequest brandDeleteWebRequest) {
    String requestId = clientParameterHelper.getRequestId();
    brandService.deleteBrand(brandCode, brandDeleteWebRequest);
    return new BaseResponse(null, null, true, requestId);
  }

  @Operation(summary = "Update active brand")
  @PostMapping(value = BrandApiPath.UPDATE, produces = MediaType.APPLICATION_JSON_VALUE)
  public BaseResponse updateBrand(@RequestPart(value = "requestBody") @Valid
  @UpdateBrandRequestValid(message = "UpdateBrandWebRequest validation failed") UpdateBrandWebRequest request,
      @RequestParam(required = false) MultipartFile brandLogo,
      @RequestParam(required = false) MultipartFile profileBanner) throws Exception {
    String requestId = clientParameterHelper.getRequestId();
    UpdateBrandRequest updateBrandRequest = ConverterUtil.toUpdateBrandRequest(request);
    brandService.updateBrand(updateBrandRequest, brandLogo, profileBanner);
    return new BaseResponse(null, null, true, requestId);
  }

  @Operation(summary = "Get brand detail by brandCode")
  @GetMapping(value = BrandApiPath.DETAIL, produces = MediaType.APPLICATION_JSON_VALUE)
  public SingleBaseResponse<BrandWebResponse> getBrandDetail(@PathVariable String brandCode) {
    String requestId = clientParameterHelper.getRequestId();
    BrandWebResponse response = brandService.getBrandDetail(brandCode);
    return new SingleBaseResponse<>(null, null, true, requestId, response);
  }

  @Operation(summary = "Retrieve brand image by brandCode or brandRequestCode")
  @GetMapping(value = BrandApiPath.IMAGE, produces = MediaType.APPLICATION_JSON_VALUE)
  public void filterBrandLogoByBrandCode(HttpServletResponse httpServletResponse,
      @RequestParam(required = false) String brandCode, @RequestParam(required = false) String brandRequestCode,
      @RequestParam(defaultValue = "true") boolean isBrandLogo) throws Exception {
    BrandLogo brandLogo = new BrandLogo();
      if(StringUtils.isEmpty(brandCode) && StringUtils.isEmpty(brandRequestCode)){
        throw new ApplicationException(ErrorCategory.REQUIRED_PARAMETER.toString(),
            BRAND_CODE_AND_BRAND_REQUEST_CODE_EMPTY);
      }
      else if(StringUtils.isNotEmpty(brandCode) && StringUtils.isNotEmpty(brandRequestCode)) {
        throw new ApplicationException(ErrorCategory.UNSPECIFIED.toString(),
            BRAND_CODE_AND_BRAND_REQUEST_CODE_SENT);
      }
      else if(StringUtils.isNotEmpty(brandCode)) {
        brandLogo = this.brandService.filterBrandLogoByBrandCode(brandCode, isBrandLogo);
      }
      else {
        brandLogo = this.brandService.filterBrandLogoByBrandRequestCode(brandRequestCode, isBrandLogo);
      }
      httpServletResponse.setHeader("Content-Type", "image/" + brandLogo.getBrandLogoFileType());
      httpServletResponse.setHeader("Content-Disposition", "inline; filename=" + brandLogo.getBrandLogoPath());
      httpServletResponse.setContentLength(brandLogo.getBrandLogoData().length);
      ServletOutputStream servletOutputStream = httpServletResponse.getOutputStream();
      servletOutputStream.write(brandLogo.getBrandLogoData());
      servletOutputStream.flush();
    }
}
