package com.gdn.aggregate.platform.module.product.app.generator;

import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.config.BeanDefinition;
import org.springframework.context.annotation.AnnotationBeanNameGenerator;
import org.springframework.util.StringUtils;

@Slf4j
public class UniqueBeanNameGenerator extends AnnotationBeanNameGenerator {

  private final String PREFIX_PACKAGE = "com.gdn.aggregate.modules";

  @Override
  protected String buildDefaultBeanName(BeanDefinition definition) {
    if (definition.getBeanClassName().startsWith(PREFIX_PACKAGE)) {
      return constructUniqueBeanName(definition);
    }
    return super.buildDefaultBeanName(definition);
  }

  private String constructUniqueBeanName(BeanDefinition definition) {
    String[] fullBeanName = definition.getBeanClassName().split("\\.");
    StringBuilder customBeanName = new StringBuilder(fullBeanName[5]);
    for (int i = 6; i < fullBeanName.length; i++) {
      customBeanName.append(StringUtils.capitalize(fullBeanName[i]));
    }
    return customBeanName.toString();
  }

}