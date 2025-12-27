package com.gdn.partners.pcu.external.web.controller;

import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.partners.pcu.external.model.BrandWipApiPath;
import com.gdn.partners.pcu.external.service.BrandWipService;
import com.gdn.partners.pcu.external.service.impl.exception.DuplicateEntryException;
import com.gdn.partners.pcu.external.validation.validator.Annotations.CreateBrandWipValid;
import com.gdn.x.productcategorybase.dto.brand.CreateBrandWipRequest;
import com.gdn.x.productcategorybase.dto.brand.CreateBrandWipResponse;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RequestPart;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;

@Slf4j
@Tag(name ="BrandWip API")
@RestController
@RequestMapping(value = BrandWipApiPath.BASE_PATH)
@Validated
public class BrandWipController {

  @Autowired
  private BrandWipService brandWipService;

  @Operation(summary ="Create a brand as internal user")
  @PostMapping(value = BrandWipApiPath.CREATE, produces = MediaType.APPLICATION_JSON_VALUE)
  @ResponseBody
  public GdnRestSingleResponse<CreateBrandWipResponse> create(
      @RequestPart("request") @Valid @CreateBrandWipValid(message = "Request invalid") CreateBrandWipRequest request,
      @RequestParam(required = false) MultipartFile brandLogo,
      @RequestParam(required = false) MultipartFile profileBanner) throws Exception {
    log.info("Create brand:{}", request);
    try {
      return this.brandWipService.create(request, brandLogo, profileBanner);
    } catch (DuplicateEntryException e) {
      return new GdnRestSingleResponse<>(e.getMessage(), null, false, null, null);
    } catch (Exception e) {
      return new GdnRestSingleResponse<>(e.getMessage(), null, false, null, null);
    }
  }
}
