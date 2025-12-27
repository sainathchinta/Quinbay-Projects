package com.gdn.partners.product.analytics.repository.helper;

import java.util.UUID;

import org.bson.Document;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.data.mongodb.core.query.Update;

import com.gdn.partners.product.analytics.model.FieldNames;

public class DataHelperTest {
  private static final String BUSINESS_PARTNER_CODE = "BUSINESS_PARTNER_CODE";
  private Document dbObject;

  @BeforeEach
  public void setUp() {
    dbObject = new Document();
    dbObject.put(FieldNames.PRIMARY_KEY_ID, UUID.randomUUID().toString());
    dbObject.put(FieldNames.BUSINESS_PARTNER_CODE, BUSINESS_PARTNER_CODE);
  }

  @Test
  public void getUpdateFromDocument() {
    Update update = DataHelper.getUpdateFromDocument(dbObject, FieldNames.PRIMARY_KEY_ID);
    Assertions.assertEquals(1, update.getUpdateObject().size());
    Assertions.assertTrue("$set".equals(update.getUpdateObject().keySet().iterator().next()));
  }
}
