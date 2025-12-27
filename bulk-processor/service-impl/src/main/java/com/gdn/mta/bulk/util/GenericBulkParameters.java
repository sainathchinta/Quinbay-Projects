package com.gdn.mta.bulk.util;

import java.util.List;
import java.util.Map;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;

public class GenericBulkParameters {

  public static final String USER_INPUT_DATA_SHEET = "Upload Template";
  public static final int HEADER_END_ROW = 14;
  public static final int FAILED_HEADER_START_ROW = 0;
  public static final int FAILED_HEADER_END_ROW = 4;
  public static final int COLUMN_START_NUMBER = 1;
  public static final int COLUMN_END_NUMBER = 41;
  public static final int FAILED_USER_INPUT_START_ROW = 4;
  public static final int C1_CATEGORY = 0;
  public static final int C2_CATEGORY = 1;
  public static final String NOT_APPLICABLE = "N/A";
  public static final String CATEGORY_TREE = "->";
  public static final String PLUS_SIGN = "+";
  public static final String SEPARATOR = "-";
  public static final String DEFAULT_UOM = "PC";
  public static final int NUMBER_OF_IMAGES = 7;
  public static final int NUMBER_OF_OTHER_ATTRIBUTES = 5;

  public static final int PRODUCT_NAME_COLUMN = 3;
  public static final int SELLER_SKU = 5;
  public static final int IMAGE_START_COLUMN = 15;


  public static final String WARNA = "warna";
  public static final String UKURAN = "ukuran";
  public static final String VARIASI = "variasi";

  public static final String WARNA_EN = "color";
  public static final String UKURAN_EN = "size";
  public static final String VARIASI_EN = "variant";


  public static final String BRAND = "Brand";
  public static final String FAMILY_COLOUR = "Family Colour";

  public static final List<String> DEFINING_ATTRIBUTES =
      ImmutableList.<String>builder().add(WARNA).add(UKURAN).add(VARIASI).build();


  public static final int HEADER_ROW = 1;
  public static final int SUB_HEADER_ROW = 3;
  public static final int C1_C2_MAPPING_COLUMN = 0;
  public static final int CN_COLUMN = 1;
  public static final int C1_NAME_COLUMN = 3;
  public static final int C2_NAME_COLUMN = 4;
  public static final int C1_UNIQUE_NAME_COLUMN = 6;
  public static final int C1_TO_C5_COMPLETE_TREE_COLUMN = 8;
  public static final int ATTRIBUTE_NAME_COLUMN = 9;
  public static final int ATTRIBUTE_VALUE_COLUMN = 10;
  public static final int CATEGORY_ATTRIBUTE_MAPPING_VALUE_COLUMN = 11;
  public static final int CN_CATEGORY_ATTRIBUTE_MAPPING_VALUE_COLUMN = 13;
  public static final int CN_CATEGORY_ATTRIBUTE_NAME_VALUE_COLUMN = 14;
  public static final int CATEGORY_WARNA_MAPPING_COLUMN = 16;
  public static final int CATEGORY_VARIASI_MAPPING_COLUMN = 18;
  public static final int CATEGORY_UKURAN_MAPPING_COLUMN = 20;
  public static final int CATEGORY_UKURAN_VALUE_MAPPING_COLUMN = 21;
  public static final int FAMILY_COLOUR_DATA_COLUMN = 23;
  public static final int CATEGORY_DESCRIPTIVE_ATTRIBUTE_MAPPING_COLUMN = 26;
  public static final int PRODUCT_TYPE_HEADER_COLUMN = 29;
  public static final int WARNA_VALUE_COLUMN = 33;
  public static final int FAMILY_COLOUR_VALUE_COLUMN = 34;
  public static final int BUNDLE_COLUMN_NUMBERS = 2;
  public static final int INSTORE_COLUMN_NUMBERS = 1;

  public static final List<Integer> SUB_HEADER_COLUMN_NUMBER =
      ImmutableList.<Integer>builder().add(C1_C2_MAPPING_COLUMN).add(CN_COLUMN)
          .add(C1_NAME_COLUMN).add(C2_NAME_COLUMN).add(C1_UNIQUE_NAME_COLUMN).add(C1_TO_C5_COMPLETE_TREE_COLUMN)
          .add(ATTRIBUTE_NAME_COLUMN).add(ATTRIBUTE_VALUE_COLUMN).add(CATEGORY_ATTRIBUTE_MAPPING_VALUE_COLUMN)
          .add(CN_CATEGORY_ATTRIBUTE_MAPPING_VALUE_COLUMN).add(CN_CATEGORY_ATTRIBUTE_NAME_VALUE_COLUMN)
          .add(CATEGORY_WARNA_MAPPING_COLUMN).add(CATEGORY_VARIASI_MAPPING_COLUMN).add(CATEGORY_UKURAN_MAPPING_COLUMN)
          .add(CATEGORY_UKURAN_VALUE_MAPPING_COLUMN).add(FAMILY_COLOUR_DATA_COLUMN)
          .add(CATEGORY_DESCRIPTIVE_ATTRIBUTE_MAPPING_COLUMN).add(PRODUCT_TYPE_HEADER_COLUMN).add(WARNA_VALUE_COLUMN)
          .add(FAMILY_COLOUR_VALUE_COLUMN).build();


  public static final String C1_C2_MAPPING = "c1->c2";
  public static final String CN = "cn";
  public static final String C1_NAME = "c1_name";
  public static final String C2_NAME = "c2_name";
  public static final String C1_UNIQUE_NAME = "c1_name";
  public static final String C1_TO_C5_COMPLETE_TREE = "c1->c2->c3->c4->c5";
  public static final String ATTRIBUTE_NAME = "attribute_name";
  public static final String ATTRIBUTE_VALUE = "attribute_value";
  public static final String CATEGORY_ATTRIBUTE_MAPPING_VALUE = "c1->c2->c3->c4->c5_attributeName";
  public static final String CN_CATEGORY_ATTRIBUTE_MAPPING_VALUE = "c1->c2->c3->c4->c5";
  public static final String CN_CATEGORY_ATTRIBUTE_NAME_VALUE = "attribute_name";
  public static final String CATEGORY_WARNA_MAPPING = "warna_c1->c2->c3->c4->c5";
  public static final String CATEGORY_VARIASI_MAPPING = "variasi_c1->c2->c3->c4->c5";
  public static final String CATEGORY_UKURAN_MAPPING = "c1->c2->c3->c4->c5";
  public static final String CATEGORY_UKURAN_VALUE_MAPPING = "attribute_value";
  public static final String FAMILY_COLOUR_DATA = "Family color";
  public static final String CATEGORY_DESCRIPTIVE_ATTRIBUTE_MAPPING = "c1->c2->c3->c4->c5";
  public static final String PRODUCT_TYPE = "Product type";
  public static final String WARNA_VALUE = "Warna";
  public static final String FAMILY_COLOUR_MAPPING_VALUE = "Family colour";

  public static final List<String> SUB_HEADER_COLUMN_VALUE =
      ImmutableList.<String>builder().add(C1_C2_MAPPING).add(CN).add(C1_NAME).add(C2_NAME)
          .add(C1_UNIQUE_NAME).add(C1_TO_C5_COMPLETE_TREE).add(ATTRIBUTE_NAME).add(ATTRIBUTE_VALUE)
          .add(CATEGORY_ATTRIBUTE_MAPPING_VALUE).add(CN_CATEGORY_ATTRIBUTE_MAPPING_VALUE)
          .add(CN_CATEGORY_ATTRIBUTE_NAME_VALUE).add(CATEGORY_WARNA_MAPPING).add(CATEGORY_VARIASI_MAPPING)
          .add(CATEGORY_UKURAN_MAPPING).add(CATEGORY_UKURAN_VALUE_MAPPING).add(FAMILY_COLOUR_DATA)
          .add(CATEGORY_DESCRIPTIVE_ATTRIBUTE_MAPPING).add(PRODUCT_TYPE).add(WARNA_VALUE)
          .add(FAMILY_COLOUR_MAPPING_VALUE).build();

  public static final int C1_C2_MAPPING_HEADER_COLUMN = 0;
  public static final int C1_NAME_HEADER_COLUMN = 3;
  public static final int C1_UNIQUE_NAME_HEADER_COLUMN = 6;
  public static final int C1_TO_C5_COMPLETE_TREE_HEADER_COLUMN = 8;
  public static final int CN_CATEGORY_ATTRIBUTE_MAPPING_VALUE_HEADER_COLUMN = 13;
  public static final int CATEGORY_WARNA_MAPPING_HEADER_COLUMN = 16;
  public static final int CATEGORY_VARIASI_MAPPING_HEADER_COLUMN = 18;
  public static final int CATEGORY_UKURAN_MAPPING_HEADER_COLUMN = 20;
  public static final int FAMILY_COLOUR_DATA_HEADER_COLUMN = 23;
  public static final int CATEGORY_DESCRIPTIVE_ATTRIBUTE_MAPPING_HEADER_COLUMN = 26;
  public static final int HANDLING_TYPE_HEADER_COLUMN = 29;
  public static final int WARNA_FAMILY_COLOUR_MAPPING_HEADER_COLUMN = 33;
  public static final int FAMILY_COLOUR_MAPPING_HEADER_COLUMN = 34;

  public static final List<Integer> HEADER_COLUMN_NUMBER =
      ImmutableList.<Integer>builder().add(C1_C2_MAPPING_HEADER_COLUMN).add(C1_NAME_HEADER_COLUMN)
          .add(C1_UNIQUE_NAME_HEADER_COLUMN).add(C1_TO_C5_COMPLETE_TREE_HEADER_COLUMN)
          .add(CN_CATEGORY_ATTRIBUTE_MAPPING_VALUE_HEADER_COLUMN).add(CATEGORY_WARNA_MAPPING_HEADER_COLUMN)
          .add(CATEGORY_VARIASI_MAPPING_HEADER_COLUMN).add(CATEGORY_UKURAN_MAPPING_HEADER_COLUMN)
          .add(FAMILY_COLOUR_DATA_HEADER_COLUMN).add(CATEGORY_DESCRIPTIVE_ATTRIBUTE_MAPPING_HEADER_COLUMN)
          .add(HANDLING_TYPE_HEADER_COLUMN).add(WARNA_FAMILY_COLOUR_MAPPING_HEADER_COLUMN).build();

  public static final String C1_C2_MAPPING_HEADER = "Input 1 - C1 to Cn mapping";
  public static final String C1_NAME_HEADER = "Input 2 - C1-C2 mapping";
  public static final String C1_UNIQUE_NAME_HEADER = "Input 3 – list of unique C1 names";
  public static final String C1_TO_C5_COMPLETE_TREE_HEADER = "Input 4 – Attribute values for Cn";
  public static final String CN_CATEGORY_ATTRIBUTE_MAPPING_VALUE_HEADER = "Input 5 – Cn to Attribute names mapping";
  public static final String CATEGORY_WARNA_MAPPING_HEADER = "Input 6 – Eligible Cn for warna";
  public static final String CATEGORY_VARIASI_MAPPING_HEADER = "Input 7 – Eligible Cn for variasi";
  public static final String CATEGORY_UKURAN_MAPPING_HEADER = "Input 8 – Ukuran values for Cn";
  public static final String FAMILY_COLOUR_DATA_HEADER = "Input 9 – Family color values";
  public static final String CATEGORY_DESCRIPTIVE_ATTRIBUTE_MAPPING_HEADER =
      "Input 10 – Eligible Cn for Descriptive attributes";
  public static final String HANDLING_TYPE_HEADER = "Input 11 - handling type";
  public static final String FAMILY_COLOUR_HEADER = "Input 12 - family color mapping";

  public static final List<String> HEADER_COLUMN_VALUE =
      ImmutableList.<String>builder().add(C1_C2_MAPPING_HEADER).add(C1_NAME_HEADER).add(C1_UNIQUE_NAME_HEADER)
          .add(C1_TO_C5_COMPLETE_TREE_HEADER).add(CN_CATEGORY_ATTRIBUTE_MAPPING_VALUE_HEADER)
          .add(CATEGORY_WARNA_MAPPING_HEADER).add(CATEGORY_VARIASI_MAPPING_HEADER).add(CATEGORY_UKURAN_MAPPING_HEADER)
          .add(FAMILY_COLOUR_DATA_HEADER).add(CATEGORY_DESCRIPTIVE_ATTRIBUTE_MAPPING_HEADER).add(HANDLING_TYPE_HEADER)
          .add(FAMILY_COLOUR_HEADER).build();

  public static final int POSSIBLE_VALUE_START_COLUMN = 4;

  public static final String MULTICOLOUR = "Multicolor";
  public static final String BLACK = "Hitam";
  public static final String BLUE = "Biru";
  public static final String GRAY = "Abu-abu";
  public static final String PURPLE = "Ungu";
  public static final String RED = "Merah";
  public static final String YELLOW = "Kuning";
  public static final String PINK = "Pink";
  public static final String ORANGE = "Orange";
  public static final String CHOCOLATE = "Coklat";
  public static final String GREEN = "Hijau";
  public static final String WHITE = "Putih";
  public static final String SILVER = "Silver";
  public static final String GOLD = "Gold";

  public static final List<String> FAMILY_COLOUR_VALUE =
      ImmutableList.<String>builder().add(MULTICOLOUR).add(BLACK).add(BLUE).add(GRAY).add(PURPLE).add(RED).add(YELLOW)
          .add(PINK).add(ORANGE).add(CHOCOLATE).add(GREEN).add(WHITE).add(SILVER).add(GOLD).build();

  public static final String BLACK_HITAM = "Black,Hitam";
  public static final String GRAY_ABU_ABU = "Gray,Abu-abu";
  public static final String MULTICOLOR_MULTICOLOR = "Multicolor,Multicolor";
  public static final String BLUE_BIRU = "Blue,Biru";
  public static final String PURPLE_UNGU = "Purple,Ungu";
  public static final String RED_MERAH = "Red,Merah";
  public static final String YELLOW_KUNING = "Yellow,Kuning";
  public static final String PINK_PINK = "Pink,Pink";
  public static final String ORANGE_ORANGE = "Orange,Orange";
  public static final String GREEN_HIJAU = "Green,Hijau";
  public static final String WHITE_PUTIH = "White,Putih";
  public static final String SILVER_SILVER = "Silver,Silver";
  public static final String GOLD_GOLD = "Gold,Gold";
  public static final String CHOCOLATE_COKLAT = "Chocolate,Coklat";
  public static final String HITAM_HITAM = "Hitam,Hitam";
  public static final String ABU_ABU_ABU_ABU = "Abu-abu,Abu-abu";
  public static final String BIRU_BIRU = "Biru,Biru";
  public static final String UNGU_UNGU = "Ungu,Ungu";
  public static final String MERAH_MERAH = "Merah,Merah";
  public static final String KUNING_KUNING = "Kuning,Kuning";
  public static final String HIJAU_HIJAU = "Hijau,Hijau";
  public static final String PUTIH_PUTIH = "Putih,Putih";
  public static final String COKLAT_COKLAT = "Coklat,Coklat";

  public static final List<String> WARNA_FAMILY_COLOUR_MAPPING =
      ImmutableList.<String>builder().add(BLACK_HITAM).add(GRAY_ABU_ABU).add(MULTICOLOR_MULTICOLOR).add(BLUE_BIRU)
          .add(PURPLE_UNGU).add(RED_MERAH).add(YELLOW_KUNING).add(PINK_PINK).add(ORANGE_ORANGE).add(GREEN_HIJAU)
          .add(WHITE_PUTIH).add(SILVER_SILVER).add(GOLD_GOLD).add(CHOCOLATE_COKLAT).add(HITAM_HITAM)
          .add(ABU_ABU_ABU_ABU).add(BIRU_BIRU).add(UNGU_UNGU).add(MERAH_MERAH).add(KUNING_KUNING).add(HIJAU_HIJAU)
          .add(PUTIH_PUTIH).add(COKLAT_COKLAT).build();

  public static final int GENERIC_PRIMARY_HEADER_ROW_NUMBER_EN = 7;
  public static final int GENERIC_PRIMARY_HEADER_ROW_NUMBER_ID = 11;
  public static final Map<Boolean, Integer> genericExcelHeaderRowNumberMap =
      ImmutableMap.of(true, GENERIC_PRIMARY_HEADER_ROW_NUMBER_EN, false, GENERIC_PRIMARY_HEADER_ROW_NUMBER_ID);

  public static final int CATEGORY_ATTRIBUTE_SHEET_INDEX = 6;

  private GenericBulkParameters() {
  }
}
