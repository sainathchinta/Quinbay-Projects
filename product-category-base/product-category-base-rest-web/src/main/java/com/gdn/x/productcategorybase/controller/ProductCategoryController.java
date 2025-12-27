package com.gdn.x.productcategorybase.controller;

import java.util.ArrayList;
import java.util.List;

import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import com.gdn.common.util.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;

import com.gdn.common.base.GdnPreconditions;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.x.productcategorybase.ProductCategoryApiPath;
import com.gdn.x.productcategorybase.controller.util.ConverterUtil;
import com.gdn.x.productcategorybase.dto.AddProductAttributesDTO;
import com.gdn.x.productcategorybase.dto.CategorySummaryDTO;
import com.gdn.x.productcategorybase.dto.CategorySummaryResponse;
import com.gdn.x.productcategorybase.dto.DescriptiveAttributeValueType;
import com.gdn.x.productcategorybase.dto.request.AddProductAttributesRequest;
import com.gdn.x.productcategorybase.dto.response.AttributeResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;
import com.gdn.x.productcategorybase.dto.response.PredefinedAllowedAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.ProductAttributeResponse;
import com.gdn.x.productcategorybase.dto.response.ProductAttributeValueResponse;
import com.gdn.x.productcategorybase.entity.Category;
import com.gdn.x.productcategorybase.entity.ProductAttribute;
import com.gdn.x.productcategorybase.entity.ProductAttributeValue;
import com.gdn.x.productcategorybase.service.MapperService;
import com.gdn.x.productcategorybase.service.ProductCategoryService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.web.bind.annotation.RestController;

@RestController
@Tag(name = "ProductCategoryController", description = "Master Product's Category Service API")
public class ProductCategoryController {
  
  @Autowired
  private ProductCategoryService prdCategoryService;

  @Autowired
  private MapperService mapperService;

  private static final Logger LOG = LoggerFactory.getLogger(ProductCategoryController.class);
  
  @RequestMapping(value = ProductCategoryApiPath.MOVE_CATEGORY_BY_PRD_CODE, method = RequestMethod.GET,
    produces = {MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE})
  @Operation(summary = "move product category by product code",
    description = "move product category by product code")
  
  public GdnRestSingleResponse<CategorySummaryResponse> movePrdCategoryByPrdCode(
      @RequestParam String storeId, @RequestParam String channelId, 
      @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @RequestParam String productCode,
      @RequestParam String categoryCode)
    throws Exception {
    GdnPreconditions.checkArgument(StringUtils.isNotEmpty(productCode), "Product code can't be null");
    GdnPreconditions.checkArgument(StringUtils.isNotEmpty(categoryCode), "Category code can't be null");
    
    CategorySummaryDTO result = 
        this.prdCategoryService.movePrdCategoryByProductCode(storeId, productCode, categoryCode);
    CategorySummaryResponse response = new CategorySummaryResponse();
    BeanUtils.copyProperties(result, response);
    return new GdnRestSingleResponse<>(response, requestId);
  }
  
  @RequestMapping(value = ProductCategoryApiPath.ADD_PRD_ATTRIBUTES_BY_PRD_CODE, method = RequestMethod.POST,
      produces = {MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE},
      consumes = {MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE})
    @Operation(summary = "add product's attributes by product code",
      description = "add product's attributes by product code except DEFINING ATTRIBUTE")
    
    public GdnRestListResponse<ProductAttributeResponse> addProductAttributesByProductCode(
        @RequestParam String storeId, @RequestParam String channelId, 
        @RequestParam String clientId, @RequestParam String requestId,
        @RequestParam String username, @RequestBody AddProductAttributesRequest request)
      throws Exception {
      GdnPreconditions.checkArgument(StringUtils.isNotEmpty(request.getProductCode()), "Product code can't be null");
      AddProductAttributesDTO attrRequest = mapperService.mapBean(request, AddProductAttributesDTO.class);
      List<ProductAttribute> result = 
          this.prdCategoryService.addProductAttributes(storeId, attrRequest);
      
      return new GdnRestListResponse<>(this.convertToPrdAttrRequest(result), null, requestId);
    }
  
  private List<ProductAttributeResponse> convertToPrdAttrRequest(List<ProductAttribute> request) throws Exception {
    List<ProductAttributeResponse> prdAttrReqList = new ArrayList<>();
    for(ProductAttribute source : request){
      ProductAttributeResponse prdAttrResult = new ProductAttributeResponse();
      BeanUtils.copyProperties(source, prdAttrResult, "product", "attribute");
      
      AttributeResponse attrResp = new AttributeResponse();
      BeanUtils.copyProperties(source.getAttribute(), attrResp, "allowedAttributeValues", "predefinedAllowedAttributeValues");
      attrResp.setAttributeType(source.getAttribute().getAttributeType().name());
      prdAttrResult.setAttribute(attrResp);
      
      List<ProductAttributeValueResponse> prdAttrValueResp = new ArrayList<>();
      for(ProductAttributeValue prdAttrValue : source.getProductAttributeValues()){
        ProductAttributeValueResponse target = new ProductAttributeValueResponse();
        BeanUtils.copyProperties(prdAttrValue, target, "productAttribute", "allowedAttributeValue");
        target.setDescriptiveAttributeValueType(
            DescriptiveAttributeValueType.valueOf(prdAttrValue.getDescriptiveAttributeValueType().name()));
        
        if(prdAttrValue.getPredefinedAllowedAttributeValue() != null){
          PredefinedAllowedAttributeValueResponse predefAllowedAttrResp = new PredefinedAllowedAttributeValueResponse();
          BeanUtils.copyProperties(prdAttrValue.getPredefinedAllowedAttributeValue(), predefAllowedAttrResp, "attribute");
          target.setPredefinedAllowedAttributeValue(predefAllowedAttrResp);
        }
        prdAttrValueResp.add(target);
      }
      prdAttrResult.setProductAttributeValues(prdAttrValueResp);
      prdAttrReqList.add(prdAttrResult);
    }
    return prdAttrReqList;
  }

  @RequestMapping(value = ProductCategoryApiPath
      .GET_MASTER_PARENT_CATEGORY_RESPONSE_BY_PRODUCT_CODE, method = RequestMethod.GET, produces
      = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "get master parent category response by productCode",
                description = "Calling from X-product")
  
  public GdnRestListResponse<CategoryResponse> getMasterParentCategoriesByProductCode(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam String username,
      @RequestParam String productCode) {
    String errorCode = StringUtils.EMPTY;
    String errorMessage = StringUtils.EMPTY;
    boolean success = false;
    List<CategoryResponse> categoryResponses = null;
    GdnPreconditions.checkArgument(StringUtils.isNotBlank(productCode),
        ProductController.PRODUCT_CODE_MUST_NOT_BE_BLANK);
    try {
      LOG.info("Get master parent categories response by productCode:{}", productCode);
      List<Category> result =
          this.prdCategoryService.getMasterParentCategoryByProductCode(storeId, productCode);
      categoryResponses = ConverterUtil.convertToCategoryResponse(result);
      success = true;
    } catch (ApplicationRuntimeException ex) {
      errorMessage = ex.getMessage();
      errorCode = ErrorCategory.UNSPECIFIED.getCode();
      LOG.error("Error while getting master parent categories response by productCode:{}",
          productCode, ex);
    } catch (Exception ex) {
      errorMessage = ErrorCategory.UNSPECIFIED.getMessage();
      errorCode = ErrorCategory.UNSPECIFIED.getCode();
      LOG.error("Error while getting master parent categories response by productCode:{}",
          productCode, ex);
    }
    return new GdnRestListResponse<>(errorMessage, errorCode, success, categoryResponses, null,
        requestId);
  }
  
}
