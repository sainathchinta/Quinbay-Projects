package com.gdn.x.productcategorybase.helper;

import java.util.ArrayList;
import java.util.List;

import com.gdn.common.util.BeanUtils;

import com.gdn.x.productcategorybase.dto.AttributeOptionDTO;
import com.gdn.x.productcategorybase.dto.allowedvalue.AllowedAttributeValueDtoRequest;
import com.gdn.x.productcategorybase.dto.allowedvalue.AllowedAttributeValueDtoResponse;
import com.gdn.x.productcategorybase.dto.allowedvalue.AllowedAttributeValueRequest;
import com.gdn.x.productcategorybase.dto.allowedvalue.AllowedAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.AttributeOptionResponse;

/**
 * Converter class that used by AllowedAttributeValueController to convert DTO object
 * 
 * @author agie.falah
 *
 */
public class AllowedAttributeValueConverter {
  
  /**
   * Convert allowed value DTO response from service into rest response
   * 
   * @param response
   * @return
   */
  public static List<AllowedAttributeValueResponse> convertAllowedValueDtoResponseToRestResponse(
      List<AllowedAttributeValueDtoResponse> response){
    List<AllowedAttributeValueResponse> result = new ArrayList<>();
    for(AllowedAttributeValueDtoResponse allowedAttrResp : response){
      AllowedAttributeValueResponse allowedAttrResult = new AllowedAttributeValueResponse();
      BeanUtils.copyProperties(allowedAttrResp, allowedAttrResult);
      result.add(allowedAttrResult);
    }
    return result;
  }
  
  /**
   * Convert allowed value request from client into allowed value DTO request that will be fit for service level
   * 
   * @param requestList
   * @return
   */
  public static List<AllowedAttributeValueDtoRequest> convertAllowedValueRestRequestToDtoRequest(
      List<AllowedAttributeValueRequest> requestList){
    List<AllowedAttributeValueDtoRequest> dtoRequestList = new ArrayList<AllowedAttributeValueDtoRequest>();
    for(AllowedAttributeValueRequest request : requestList){
      AllowedAttributeValueDtoRequest dtoRequest = new AllowedAttributeValueDtoRequest();
      BeanUtils.copyProperties(request, dtoRequest);
      dtoRequestList.add(dtoRequest);
    }
    return dtoRequestList;
  }
  
  /**
   * Convert attributeOptionDTO into AttributeOptionResponse
   * @param options
   * @return
   */
  public static List<AttributeOptionResponse> convertIntoAttributeOptionResponse(List<AttributeOptionDTO> options) {
    List<AttributeOptionResponse> optionsResponse = new ArrayList<>();
    for(AttributeOptionDTO item : options) {
      AttributeOptionResponse target = new AttributeOptionResponse();
      BeanUtils.copyProperties(item,target); 
      optionsResponse.add(target);
    }
    return optionsResponse;
  }
  
}   
