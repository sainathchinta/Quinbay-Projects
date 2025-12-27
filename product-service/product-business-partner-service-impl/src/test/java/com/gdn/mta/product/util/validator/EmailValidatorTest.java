package com.gdn.mta.product.util.validator;

import java.util.ArrayList;
import java.util.Arrays;

import org.apache.commons.collections4.CollectionUtils;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.gdn.mta.product.commons.constant.RestrictedKeywordFieldNames;
import com.gda.mta.product.dto.RestrictedKeywordsByField;

/**
 * Created by Alok on 7/1/16.
 */
public class EmailValidatorTest {

  @Test
  public void testisValidEmailAddress() {
    RestrictedKeywordsByField restrictedKeywordsByField1 =
        new RestrictedKeywordsByField(RestrictedKeywordFieldNames.PRODUCT_NAME.name(), new ArrayList<>());
    RestrictedKeywordsByField restrictedKeywordsByField2 =
        new RestrictedKeywordsByField(RestrictedKeywordFieldNames.PRODUCT_NAME.name(), new ArrayList<>());
    RestrictedKeywordsByField restrictedKeywordsByField3 =
        new RestrictedKeywordsByField(RestrictedKeywordFieldNames.PRODUCT_NAME.name(), new ArrayList<>());
    EmailValidator.getEmailAddressFromProductFields("abc@gmail.com", restrictedKeywordsByField1);
    EmailValidator.getEmailAddressFromProductFields("abv@@gmail.com", restrictedKeywordsByField2);
    EmailValidator.getEmailAddressFromProductFields("abc", restrictedKeywordsByField3);
    restrictedKeywordsByField1.setKeywords(Arrays.asList("abc@gmail.com"));
    EmailValidator.getEmailAddressFromProductFields("abc@gmail.com", restrictedKeywordsByField1);
    Assertions.assertTrue(CollectionUtils.isNotEmpty(restrictedKeywordsByField1.getKeywords()));
    Assertions.assertTrue(CollectionUtils.isEmpty(restrictedKeywordsByField2.getKeywords()));
    Assertions.assertTrue(CollectionUtils.isEmpty(restrictedKeywordsByField3.getKeywords()));
    Assertions.assertEquals("abc@gmail.com", restrictedKeywordsByField1.getKeywords().get(0));
  }
  
  @SuppressWarnings("unused")
  @Test
  public void constructorTest() throws Exception {
    EmailValidator emailValidator = new EmailValidator();
  }

}
