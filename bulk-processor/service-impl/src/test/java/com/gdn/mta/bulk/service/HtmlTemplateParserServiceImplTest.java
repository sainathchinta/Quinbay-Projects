package com.gdn.mta.bulk.service;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.MockitoAnnotations;

import java.util.HashMap;

public class HtmlTemplateParserServiceImplTest {

  @InjectMocks
  private HtmlTemplateParserServiceImpl htmlTemplateParserService;
  private static final String NAME = "name";

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
  }

  @Test
  public void parseVelocityTemplateToHtmlTest() {
    String result = htmlTemplateParserService.parseVelocityTemplateToHtml("test", "Hey $test",
        new HashMap<>() {{
          put("test", NAME);
        }});
    Assertions.assertTrue(result.contains(NAME));
  }

}
