package com.gdn.partners.pdt.controller.configuration.distribution;

import java.util.ArrayList;
import java.util.List;

import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.stereotype.Controller;
import org.springframework.util.StringUtils;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;

import com.gdn.common.base.GdnPreconditions;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.partners.pdt.dto.configuration.distribution.AutoDistributionConfigurationRequest;
import com.gdn.partners.pdt.dto.configuration.distribution.AutoDistributionConfigurationResponse;
import com.gdn.partners.pdt.dto.configuration.distribution.CreateAutoDistributionConfigurationRequest;
import com.gdn.partners.pdt.dto.configuration.distribution.UpdateAutoDistributionConfigurationRequest;
import com.gdn.partners.pdt.entity.configuration.distribution.AutoDistributionConfiguration;
import com.gdn.partners.pdt.model.configuration.distribution.PriorityType;
import com.gdn.partners.pdt.service.configuration.distribution.AutoDistributionConfigurationService;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;

@Controller
@RequestMapping(value = AutoDistributionConfigurationControllerPath.BASE_PATH)
@Tag(name = "Auto Distribution Configuration", description = "Auto Distribution Configuration")
public class AutoDistributionConfigurationController {

  @Autowired
  private AutoDistributionConfigurationService autoDistributionConfigurationService;

  @RequestMapping(value = AutoDistributionConfigurationControllerPath.FILTER_VENDOR_CODE, method = RequestMethod.GET,
      produces = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "filter by vendor code", description = "filter by vendor code")
  @ResponseBody
  public GdnRestListResponse<AutoDistributionConfigurationResponse> filterByVendorCode(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @RequestParam String vendorCode) throws Exception {
    List<AutoDistributionConfigurationResponse> autoDistributionConfigurationResponses =
        this.filterByVendorCode(vendorCode);
    return new GdnRestListResponse<AutoDistributionConfigurationResponse>(autoDistributionConfigurationResponses,
        new PageMetaData(autoDistributionConfigurationResponses.size(), 0,
            autoDistributionConfigurationResponses.size()), requestId);
  }

  @RequestMapping(value = AutoDistributionConfigurationControllerPath.CREATE, method = RequestMethod.POST,
      consumes = {MediaType.APPLICATION_JSON_VALUE}, produces = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "create", description = "create")
  @ResponseBody
  public GdnBaseRestResponse create(@RequestParam String storeId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam String requestId, @RequestParam String username,
      @RequestBody CreateAutoDistributionConfigurationRequest request) throws Exception {
    GdnPreconditions.checkArgument(!StringUtils.isEmpty(request.getVendorCode()),
        AutoDistributionConfigurationControllerErrorMessage.VENDOR_CODE_MUST_NOT_BE_BLANK);
    GdnPreconditions.checkArgument(request.getAutoDistributionConfigurations() != null
        && !request.getAutoDistributionConfigurations().isEmpty(),
        AutoDistributionConfigurationControllerErrorMessage.AUTO_DISTRIBUTION_CONFIGURATIONS_MUST_NOT_BE_BLANK);
    this.validateCreateOrUpdate(request.getAutoDistributionConfigurations());
    this.autoDistributionConfigurationService.create(request.getVendorCode(),
        this.generateAutoDistributionConfigurations(request.getAutoDistributionConfigurations()));
    return new GdnBaseRestResponse(requestId);
  }

  @RequestMapping(value = AutoDistributionConfigurationControllerPath.UPDATE, method = RequestMethod.POST,
      consumes = {MediaType.APPLICATION_JSON_VALUE}, produces = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "update", description = "update")
  @ResponseBody
  public GdnBaseRestResponse update(@RequestParam String storeId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam String requestId, @RequestParam String username,
      @RequestBody UpdateAutoDistributionConfigurationRequest request) throws Exception {
    GdnPreconditions.checkArgument(!StringUtils.isEmpty(request.getVendorCode()),
        AutoDistributionConfigurationControllerErrorMessage.VENDOR_CODE_MUST_NOT_BE_BLANK);
    GdnPreconditions.checkArgument(request.getAutoDistributionConfigurations() != null
        && !request.getAutoDistributionConfigurations().isEmpty(),
        AutoDistributionConfigurationControllerErrorMessage.AUTO_DISTRIBUTION_CONFIGURATIONS_MUST_NOT_BE_BLANK);
    this.validateCreateOrUpdate(request.getAutoDistributionConfigurations());
    this.autoDistributionConfigurationService.update(request.getVendorCode(),
        this.generateAutoDistributionConfigurations(request.getAutoDistributionConfigurations()));
    return new GdnBaseRestResponse(requestId);
  }

  private List<AutoDistributionConfigurationResponse> filterByVendorCode(String vendorCode) throws Exception {
    List<AutoDistributionConfiguration> autoDistributionConfigurations =
        this.autoDistributionConfigurationService.findByVendorCode(vendorCode);
    return this.generateAutoDistributionConfigurationResponses(autoDistributionConfigurations);
  }

  private AutoDistributionConfiguration generateAutoDistributionConfiguration(
      AutoDistributionConfigurationRequest request) {
    AutoDistributionConfiguration autoDistributionConfiguration = new AutoDistributionConfiguration();
    autoDistributionConfiguration.setPriority(PriorityType.valueOf(request.getPriorityType()).getPriority());
    autoDistributionConfiguration.setPriorityType(PriorityType.valueOf(request.getPriorityType()));
    autoDistributionConfiguration.setPriorityValue(request.getPriorityValue());
    return autoDistributionConfiguration;
  }

  private List<AutoDistributionConfiguration> generateAutoDistributionConfigurations(
      List<AutoDistributionConfigurationRequest> request) {
    List<AutoDistributionConfiguration> autoDistributionConfigurations = new ArrayList<AutoDistributionConfiguration>();
    for (AutoDistributionConfigurationRequest item : request) {
      autoDistributionConfigurations.add(this.generateAutoDistributionConfiguration(item));
    }
    return autoDistributionConfigurations;
  }

  private AutoDistributionConfigurationResponse generateAutoDistributionConfigurationResponse(
      AutoDistributionConfiguration autoDistributionConfiguration) throws Exception {
    AutoDistributionConfigurationResponse autoDistributionConfigurationResponse = null;
    if (autoDistributionConfiguration != null) {
      autoDistributionConfigurationResponse = new AutoDistributionConfigurationResponse();
      BeanUtils.copyProperties(autoDistributionConfiguration, autoDistributionConfigurationResponse);
    }
    return autoDistributionConfigurationResponse;
  }

  private List<AutoDistributionConfigurationResponse> generateAutoDistributionConfigurationResponses(
      List<AutoDistributionConfiguration> autoDistributionConfigurations) throws Exception {
    List<AutoDistributionConfigurationResponse> autoDistributionConfigurationResponses =
        new ArrayList<AutoDistributionConfigurationResponse>();
    for (AutoDistributionConfiguration autoDistributionConfiguration : autoDistributionConfigurations) {
      autoDistributionConfigurationResponses.add(this
          .generateAutoDistributionConfigurationResponse(autoDistributionConfiguration));
    }
    return autoDistributionConfigurationResponses;
  }

  private void validateCreateOrUpdate(List<AutoDistributionConfigurationRequest> autoDistributionConfigurationRequests)
      throws Exception {
    for (AutoDistributionConfigurationRequest autoDistributionConfigurationRequest : autoDistributionConfigurationRequests) {
      GdnPreconditions.checkArgument(!StringUtils.isEmpty(autoDistributionConfigurationRequest.getPriorityType()),
          AutoDistributionConfigurationControllerErrorMessage.PRIORITY_TYPE_MUST_NOT_BE_BLANK);
      GdnPreconditions.checkArgument(!StringUtils.isEmpty(autoDistributionConfigurationRequest.getPriorityValue()),
          AutoDistributionConfigurationControllerErrorMessage.PRIORITY_VALUE_MUST_NOT_BE_BLANK);
    }
  }

}
