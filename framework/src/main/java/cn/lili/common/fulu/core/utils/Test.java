package cn.lili.common.fulu.core.utils;

import cn.lili.common.fulu.core.MethodConst;
import cn.lili.common.fulu.model.*;
import cn.lili.common.fulu.sdk.DefaultOpenApiClient;
import cn.lili.modules.store.entity.dto.FuLuConfigureDTO;
import com.alibaba.fastjson.JSON;
import com.google.gson.reflect.TypeToken;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.*;
import java.util.concurrent.*;

public class Test {
    private final static String APP_KEY = "lzHi7ewuTkplx2ePX0eQzN65TIFRk1zFClcoj1Jim/MHmu0X7ZqxMtSLENhvr1xD";
    private final static Logger LOGGER = LoggerFactory.getLogger(Test.class);
    private final static String SYS_SECRET = "9da68b0f8bcb470e84c3d30e343727e2";
    private final static String URL = "http://openapi.fulu.com/api/getway";

//    private final static String APP_KEY = "i4esv1l+76l/7NQCL3QudG90Fq+YgVfFGJAWgT+7qO1Bm9o/adG/1iwO2qXsAXNB";
//    private final static Logger LOGGER = LoggerFactory.getLogger(Test.class);
//    private final static String SYS_SECRET = "0a091b3aa4324435aab703142518a8f7";
//    private final static String URL = "http://pre.openapi.fulu.com/api/getway";

    private static String repeat(String ch, int num) {
        StringBuilder str = new StringBuilder();
        for (int i = 0; i < num; i++) {
            str.append(ch);
        }
        return str.toString();
    }

    private static void waitFor(Future<String> future) throws Exception {
        while (!future.isDone()) {
            TimeUnit.MILLISECONDS.sleep(500);
        }
        LOGGER.info("excuteAsync：\n{}", future.get());
    }

    /**
     * 卡密下单
     *
     * @throws Exception
     */
    public static void cardOrderAddTest(FuLuConfigureDTO fuLuConfigureDTO, Integer productId, Integer buyNum,String orderSn) throws Exception {
        LOGGER.info("\n卡密下单{}", repeat("=", 100));
        DefaultOpenApiClient client =
                new DefaultOpenApiClient(URL, fuLuConfigureDTO.getAppMerchantKey(), fuLuConfigureDTO.getAppSecretKey(), MethodConst.OPEN_API_CARD_ORDER_ADD);
        InputCardOrderDto dto = new InputCardOrderDto();
        dto.setProductId(productId);
        dto.setCustomerOrderNo(orderSn);
        dto.setBuyNum(buyNum);

        client.setBizObject(dto);
        LOGGER.info("excute：\n{}\n{}", client.excute(), repeat("-", 100));
        dto.setCustomerOrderNo(UUID.randomUUID().toString());
        waitFor(client.excuteAsync());
    }

    /**
     * 直充下单
     *
     * @throws Exception
     */
    public static void directOrderAddTest(FuLuConfigureDTO fuLuConfigureDTO, Integer productId, Integer buyNum,String qrCode,String orderSn) throws Exception {
        LOGGER.info("\n直充下单{}", repeat("=", 100));
        DefaultOpenApiClient client =
                new DefaultOpenApiClient(URL, fuLuConfigureDTO.getAppMerchantKey(), fuLuConfigureDTO.getAppSecretKey(), MethodConst.OPEN_API_DIRECT_ORDER_ADD);
        InputDirectOrderDto dto = new InputDirectOrderDto();
        dto.setProductId(productId);
        dto.setBuyNum(buyNum);
        client.setBizObject(dto);
        int count = 1;

        for (String chargeAccount : Arrays.asList(qrCode)) {
            dto.setChargeAccount(chargeAccount);
            dto.setCustomerOrderNo(orderSn);

            LOGGER.info("excute：\n{}\n{}", client.excute(), repeat("-", 100));
            if (count++ >= 3) {
                waitFor(client.excuteAsync());
            } else {
                LOGGER.info("excuteAsync：\n{}\n{}", client.excuteAsync().get(), repeat("-", 100));
            }

        }
    }

    /**
     * 手机号归属地
     *
     * @throws Exception
     */
    public void matchPhoneProducGetTest(FuLuConfigureDTO fuLuConfigureDTO) throws Exception {
        LOGGER.info("\n手机号归属地{}", repeat("=", 100));
        DefaultOpenApiClient client = new DefaultOpenApiClient(URL, fuLuConfigureDTO.getAppMerchantKey(), fuLuConfigureDTO.getAppSecretKey(), MethodConst.OPEN_API_CHECK_PHONE);
        InputMatchPhoneProductListDto dto = new InputMatchPhoneProductListDto();
        dto.setFaceValue(50D);

        int count = 1;
        for (String phone : Arrays.asList("15972368779", "13971553804")) {
            dto.setPhone(phone);

            client.setBizObject(dto);

            LOGGER.info("excute：\n{}\n{}", client.excute(), repeat("-", 100));
            if (count++ >= 2) {
                waitFor(client.excuteAsync());
            } else {
                LOGGER.info("excuteAsync：\n{}\n{}", client.excuteAsync().get(), repeat("-", 100));
            }

        }
    }

    /**
     * 订单查单
     *
     * @throws Exception
     */
    public void orderInfoGetTest(FuLuConfigureDTO fuLuConfigureDTO) throws Exception {
        LOGGER.info("\n订单查单{}", repeat("=", 100));
        DefaultOpenApiClient client = new DefaultOpenApiClient(URL, fuLuConfigureDTO.getAppMerchantKey(), fuLuConfigureDTO.getAppSecretKey(), MethodConst.OPEN_API_ORDER_GET);
        InputOrderGetDto dto = new InputOrderGetDto();
        client.setBizObject(dto);
        int count = 1;

        for (String customerOrderNo : Arrays
                .asList("0d19f8e4-5af3-490d-a8d8-47fd457da7de", "31b6b96b-a21e-4bc4-bc0c-6e77a2ffb698")) {
            dto.setCustomerOrderNo(customerOrderNo);

            LOGGER.info("excute：\n{}\n{}", client.excute(), repeat("-", 100));
            if (count++ >= 2) {
                Future<String> future = client.excuteAsync();
                waitFor(future);
                Map<String, String> result = JSONUtil.fromJSON(future.get(), new TypeToken<Map>() {
                });
                Map<String, Object> resultMap = JSONUtil.fromJSON(result.get("result"), new TypeToken<Map<String, Object>>() {
                });

                List<Map<String, String>> cardList = (List<Map<String, String>>) resultMap.get("cards");
                StringBuilder decodeStr = new StringBuilder();
                for (Map<String, String> map : cardList) {
                    decodeStr.append("card_number：").append(map.get("card_number")).append("；desc_card_number：")
                            .append(CardUtil.cardDecode(map.get("card_number"), SYS_SECRET.getBytes("UTF-8")));
                    decodeStr.append("  card_pwd：").append(map.get("card_pwd")).append("；card_pwd：")
                            .append(CardUtil.cardDecode(map.get("card_pwd"), SYS_SECRET.getBytes("UTF-8")));
                    decodeStr.append("\n");
                }

                System.out.println(decodeStr.toString());
                System.out.println(CardUtil.cardEncode("CD10002502019061217430016421", SYS_SECRET.getBytes("UTF-8")));
            } else {
                LOGGER.info("excuteAsync：\n{}\n{}", client.excuteAsync().get(), repeat("-", 100));
            }

        }
    }

    /**
     * 话费下单
     *
     * @throws Exception
     */
    public void phoneOrderAddTest(FuLuConfigureDTO fuLuConfigureDTO) throws Exception {
        LOGGER.info("\n话费下单{}", repeat("=", 100));
        DefaultOpenApiClient client =
                new DefaultOpenApiClient(URL, fuLuConfigureDTO.getAppMerchantKey(), fuLuConfigureDTO.getAppSecretKey(), MethodConst.OPEN_API_PHONE_ORDER_ADD);
        InputPhoneOrderDto dto = new InputPhoneOrderDto();
        dto.setChargeValue(Double.valueOf(50));
        int count = 1;

        for (String chargePhone : Arrays.asList("15972368779", "13971553804")) {
            dto.setCustomerOrderNo(UUID.randomUUID().toString());
            dto.setChargePhone(chargePhone);
            client.setBizObject(dto);

            LOGGER.info("excute：\n{}\n{}", client.excute(), repeat("-", 100));
            dto.setCustomerOrderNo(UUID.randomUUID().toString());
            if (count++ >= 2) {
                waitFor(client.excuteAsync());
            } else {
                LOGGER.info("excuteAsync：\n{}\n{}", client.excuteAsync().get(), repeat("-", 100));
            }

        }
    }


    /**
     * 获得商品信息
     *
     * @throws Exception
     */
    public static Map<String, Object> productInfoGetTest(FuLuConfigureDTO fuLuConfigureDTO,String productIdS) throws Exception {
        LOGGER.info("\n获得商品信息{}", repeat("=", 100));
        DefaultOpenApiClient client = new DefaultOpenApiClient(URL, fuLuConfigureDTO.getAppMerchantKey(), fuLuConfigureDTO.getAppSecretKey(), MethodConst.OPEN_API_GOODS_GET);
        InputProductDto dto = new InputProductDto();
        int count = 1;
        Map<String, Object> maps = new HashMap();
        for (String productId : Arrays.asList(productIdS)) {
            dto.setProductId(productId);
            client.setBizObject(dto);
            maps = (Map) JSON.parse(client.excute());
            LOGGER.info("excute：\n{}\n{}", client.excute(), repeat("-", 100));
            if (count++ >= 10) {
                waitFor(client.excuteAsync());
            } else {
                LOGGER.info("excuteAsync：\n{}\n{}", client.excuteAsync().get(), repeat("-", 100));
            }
        }
        return maps;
    }

    /**
     * 获得商品模板信息
     *
     * @throws Exception
     */
    public void productTemplateGetTest(FuLuConfigureDTO fuLuConfigureDTO) throws Exception {
        LOGGER.info("\n获得商品模板信息{}", repeat("=", 100));
        DefaultOpenApiClient client =
                new DefaultOpenApiClient(URL, fuLuConfigureDTO.getAppMerchantKey(), fuLuConfigureDTO.getAppSecretKey(), MethodConst.OPEN_API_GOODS_TEMPLATE_GET);
        InputProductTemplateDto dto = new InputProductTemplateDto();
        dto.setTemplateId("e1dac0ea-dc86-4c9d-a778-c9e19203ecb8");
        client.setBizObject(dto);
        LOGGER.info("excute：\n{}\n{}", client.excute(), repeat("-", 100));
        waitFor(client.excuteAsync());
    }

    /**
     * 流量下单
     *
     * @throws Exception
     */
    public void trafficOrderAddTest(FuLuConfigureDTO fuLuConfigureDTO) throws Exception {
        LOGGER.info("\n流量下单{}", repeat("=", 100));
        DefaultOpenApiClient client =
                new DefaultOpenApiClient(URL, fuLuConfigureDTO.getAppMerchantKey(), fuLuConfigureDTO.getAppSecretKey(), MethodConst.OPEN_API_TRAFFIC_ORDER_ADD);
        InputTrafficOrderDto dto = new InputTrafficOrderDto();
        dto.setChargeValue(Double.valueOf(1024));
        dto.setPacketKind(4);

        int count = 1;
        for (String chargePhone : Arrays.asList("15972368779", "13971553804")) {
            client.setBizObject(dto);
            dto.setChargePhone(chargePhone);
            dto.setCustomerOrderNo(UUID.randomUUID().toString());

            LOGGER.info("excute：\n{}\n{}", client.excute(), repeat("-", 100));
            dto.setCustomerOrderNo(UUID.randomUUID().toString());
            if (count++ >= 2) {
                waitFor(client.excuteAsync());
            } else {
                LOGGER.info("excuteAsync：\n{}\n{}", client.excuteAsync().get(), repeat("-", 100));
            }

        }
    }

    /**
     * 用户信息
     *
     * @throws Exception
     */
    public static void userInfoGetTest(FuLuConfigureDTO fuLuConfigureDTO) throws Exception {
        LOGGER.info("\n用户信息{}", repeat("=", 100));
        DefaultOpenApiClient client =
                new DefaultOpenApiClient(URL, fuLuConfigureDTO.getAppMerchantKey(), fuLuConfigureDTO.getAppSecretKey(), MethodConst.OPEN_API_USER_INFO_GET);
        InputUserDto dto = new InputUserDto();
        client.setBizObject(dto);

        LOGGER.info("excute：\n{}\n{}", client.excute(), repeat("-", 100));
        waitFor(client.excuteAsync());
    }


    /**
     * 用户信息：高并发场景
     *
     * @throws Exception
     */
    public static void userInfoGetTest2(FuLuConfigureDTO fuLuConfigureDTO) throws Exception {
        LOGGER.info("\n用户信息：高并发场景{}", repeat("=", 100));
        Executor executor = Executors.newCachedThreadPool();

        final int count = 10;
        final CountDownLatch downLatch = new CountDownLatch(count);

        for (int i = 0; i < count; ++i) {
            executor.execute(new Runnable() {
                @Override
                public void run() {
                    try {
                        String name = Thread.currentThread().getName();
                        LOGGER.info("\n线程名：{}{}", name, repeat("+", 100));

                        DefaultOpenApiClient client =
                                new DefaultOpenApiClient(URL, fuLuConfigureDTO.getAppMerchantKey(), fuLuConfigureDTO.getAppSecretKey(), MethodConst.OPEN_API_USER_INFO_GET);
                        InputUserDto dto = new InputUserDto();
                        client.setBizObject(dto);

                        LOGGER.info("\n线程名：{}\n{}\n{}", name, client.excute(), repeat("+", 100));
                        waitFor(client.excuteAsync());
                    } catch (Exception ex) {
                        LOGGER.error(ex.getMessage(), ex);
                    } finally {
                        downLatch.countDown();
                    }

                }
            });
        }

        downLatch.await();
    }


    /* 获得商品信息列表
     *
     * @throws Exception
     */
    public static String getGoodsInfoList(FuLuConfigureDTO fuLuConfigureDTO) throws Exception {
        LOGGER.info("\n获得商品信息{}", repeat("=", 100));
        DefaultOpenApiClient client = new DefaultOpenApiClient(URL, fuLuConfigureDTO.getAppMerchantKey(), fuLuConfigureDTO.getAppSecretKey(), MethodConst.OPEN_API_GOODS_LIST);
        InputProductDto dto = new InputProductDto();
        client.setBizObject(dto);
        return client.excute().toString();
    }



    public static void main(String[] args) {
        try {
//            productInfoGetTest();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

}
