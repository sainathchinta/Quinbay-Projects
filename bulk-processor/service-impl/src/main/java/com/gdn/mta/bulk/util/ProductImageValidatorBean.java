package com.gdn.mta.bulk.util;

import java.io.File;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.bulk.config.SystemParameter;
import com.gdn.partners.bulk.util.ProductImageValidator;

@Service
public class ProductImageValidatorBean implements ProductImageValidator {

  @Autowired
  private SystemParameter sysParam;
  
  @Override
  public void validateImages(File imageFile){
    if(imageFile.length() > sysParam.getImageMaxSize()){
      throw new ApplicationRuntimeException(ErrorCategory.INVALID_FORMAT, 
          imageFile.getName() + " size must less than " + sysParam.getImageMaxSize() + " byte, " 
              + "current size is: " + imageFile.length() + " byte");
    }
  }
}
