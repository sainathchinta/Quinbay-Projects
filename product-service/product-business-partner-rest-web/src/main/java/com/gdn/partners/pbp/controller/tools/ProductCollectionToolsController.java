package com.gdn.partners.pbp.controller.tools;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestController;

import com.gda.mta.product.dto.StringListRequest;
import com.gdn.common.base.GdnPreconditions;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.partners.pbp.service.tools.ProductCollectionToolsService;
import com.gdn.x.productcategorybase.dto.request.AddProductAttributesRequest;

import io.swagger.v3.oas.annotations.tags.Tag;
import io.swagger.v3.oas.annotations.Operation;

@RestController
@Tag(name = "Product Collection Tools Controller", description = "API to do data correction")
public class ProductCollectionToolsController {
  
  @Autowired
  private ProductCollectionToolsService productCollectionToolsService;

  @RequestMapping(value = ProductCollectionToolsPath.UPDATE_STATE, method = RequestMethod.POST, produces = {
      MediaType.APPLICATION_JSON_VALUE})
  @ResponseBody
  @Operation(summary = "Compare state and do sync between prd collection and prd bp",
      description = "use fullReload=true to reload from sysparam.properties, otherwise reload from redis")
  public GdnBaseRestResponse syncState(
      @RequestParam String storeId, 
      @RequestParam String channelId, 
      @RequestParam String clientId, 
      @RequestParam String username,
      @RequestParam String requestId, 
      @RequestParam String specifiedState, 
      @RequestBody StringListRequest productCodes)
      throws Exception {
    
    try{
      GdnPreconditions.checkArgument(CollectionUtils.isNotEmpty(productCodes), "Product code can't be null/empty");
      productCollectionToolsService.syncState(productCodes, specifiedState, storeId, username);
      return new GdnBaseRestResponse(requestId);
    } catch(Exception e){
      return new GdnBaseRestResponse(e.getMessage(), null, false, requestId);
    }
  }

  @RequestMapping(value = ProductCollectionToolsPath.MOVE_CATEGORY, method = RequestMethod.GET, produces = {
      MediaType.APPLICATION_JSON_VALUE})
  @ResponseBody
  @Operation(summary = "Move product collection category and PCB master category",
      description = "Move product collection category and PCB master category")
  public GdnBaseRestResponse moveCategoryByProductCode(
      @RequestParam String storeId, 
      @RequestParam String channelId, 
      @RequestParam String clientId, 
      @RequestParam String username,
      @RequestParam String requestId, 
      @RequestParam String productCode,
      @RequestParam String categoryCode)
      throws Exception {
    try{
      GdnPreconditions.checkArgument(StringUtils.isNotEmpty(productCode), "Product code can't be null");
      GdnPreconditions.checkArgument(StringUtils.isNotEmpty(categoryCode), "Category code can't be null");
      
      productCollectionToolsService.moveProductCollectionCategory(requestId, username, storeId, productCode, categoryCode);
      return new GdnBaseRestResponse(requestId);
    } catch(Exception e){
      return new GdnBaseRestResponse(e.getMessage(), null, false, requestId);
    }
  }
  
  @RequestMapping(value = ProductCollectionToolsPath.ADD_PRD_ATTRIBUTES_BY_PRD_CODE, method = RequestMethod.POST,
      produces = {MediaType.APPLICATION_JSON_VALUE})
  @ResponseBody
  @Operation(summary = "Add product's attributes to PCB and X-Product",
      description = "Only for DESCRIPTIVE_ATTRIBUTE AND PREDEFINED_ATTRIBUTE")
  public GdnBaseRestResponse addProductAttributes(
      @RequestParam String storeId, 
      @RequestParam String channelId, 
      @RequestParam String clientId, 
      @RequestParam String username,
      @RequestParam String requestId, 
      @RequestBody AddProductAttributesRequest request)
      throws Exception {
    try{
      GdnPreconditions.checkArgument(StringUtils.isNotEmpty(request.getProductCode()), 
          "Product code can't be null");
      return productCollectionToolsService.addProductAttributesByProductCode(requestId, username, request);
    } catch(Exception e){
      return new GdnBaseRestResponse(e.getMessage(), null, false, requestId);
    }
  }
  
}
