package cn.lili.modules.wechat.serviceimpl;

import cn.hutool.http.HttpUtil;
import cn.hutool.json.JSONArray;
import cn.hutool.json.JSONObject;
import cn.hutool.json.JSONUtil;
import cn.lili.common.enums.ClientTypeEnum;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.utils.HttpUtils;
import cn.lili.modules.order.order.entity.enums.OrderStatusEnum;
import cn.lili.modules.wechat.entity.dos.WechatMPMessage;
import cn.lili.modules.wechat.entity.enums.WechatMessageItemEnums;
import cn.lili.modules.wechat.mapper.WechatMPMessageMapper;
import cn.lili.modules.wechat.service.WechatMPMessageService;
import cn.lili.modules.wechat.util.WechatAccessTokenUtil;
import cn.lili.modules.wechat.util.WechatMessageUtil;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * 微信小程序消息订阅 业务实现
 *
 * @author Chopper
 */
@Service
@Slf4j
@Transactional(rollbackFor = Exception.class)
public class WechatMPMessageServiceImpl extends ServiceImpl<WechatMPMessageMapper, WechatMPMessage> implements WechatMPMessageService {
    @Autowired
    private WechatAccessTokenUtil wechatAccessTokenUtil;

    /**
     * get 获取所有的模版
     */
    private final String allMsgTpl = "https://api.weixin.qq.com/wxaapi/newtmpl/gettemplate?access_token=";
    /**
     * 获取keyid
     */
    private final String keywords = "https://api.weixin.qq.com/wxaapi/newtmpl/getpubtemplatekeywords?access_token=";
    /**
     * post 删除模版 添加模版 获取模版id
     */
    private final String delMsgTpl = "https://api.weixin.qq.com/wxaapi/newtmpl/deltemplate?access_token=";
    /**
     * post 添加模版
     */
    private final String addTpl = "https://api.weixin.qq.com/wxaapi/newtmpl/addtemplate?access_token=";

    @Override
    public void init() {
        this.baseMapper.deleteAll();
        try {
            String accessToken = wechatAccessTokenUtil.cgiAccessToken(ClientTypeEnum.WECHAT_MP);
            //获取已有模版，删除
            String context = HttpUtil.get(allMsgTpl + accessToken);

            log.info("获取全部模版：{}", context);
            JSONObject jsonObject = new JSONObject(context);
            WechatMessageUtil.wechatHandler(jsonObject);
            List<String> oldList = new ArrayList<>();
            if (jsonObject.containsKey("data")) {
                jsonObject.getJSONArray("data").forEach(item -> {
                    oldList.add(JSONUtil.parseObj(item).getStr("priTmplId"));
                });
            }
            if (oldList.size() != 0) {
                oldList.forEach(templateId -> {
                    Map<String, Object> params = new HashMap<>(1);
                    params.put("priTmplId", templateId);
                    String message = WechatMessageUtil.wechatHandler(HttpUtil.post(delMsgTpl + accessToken, params));
                    log.info("删除模版请求:{},删除模版响应：{}", params, message);
                });
            }

            //加入数据
            List<WechatMPMessageData> tmpList = initData();
            tmpList.forEach(tplData -> {
                WechatMPMessage wechatMPMessage = new WechatMPMessage();

                Map params = new HashMap<>(16);
                params.put("tid", tplData.getTid());
                //获取微信消息订阅keys
                String keywordsItems = WechatMessageUtil.wechatHandler(HttpUtil.get(keywords + accessToken, params));
                JSONArray jsonArray = new JSONObject(keywordsItems).getJSONArray("data");
                List<WechatMessageKeyword> keywordArray = jsonArray.toList(WechatMessageKeyword.class);

                log.info("keywords:" + keywordArray);
                //存放约定好的kids
                List<String> kids = new ArrayList<>(tplData.keyWord.size());
                List<String> kidTexts = new ArrayList<>(tplData.keyWord.size());
                keywordArray:
                for (WechatMessageItemEnums enums : tplData.getKeyWord()) {

                    for (String tplKey : enums.getText()) {
                        for (WechatMessageKeyword wechatMessageKeyword : keywordArray) {
                            if (wechatMessageKeyword.getName().equals(tplKey)) {
                                kids.add(wechatMessageKeyword.getKid());
                                kidTexts.add(wechatMessageKeyword.getRule() + wechatMessageKeyword.getKid());
                                continue keywordArray;
                            }
                        }
                    }
                }
                params = new HashMap<>(4);
                params.put("tid", tplData.getTid());
                params.put("kidList", kids);
                params.put("sceneDesc", tplData.getSceneDesc());
                String content = HttpUtils.doPostWithJson(addTpl + accessToken, params);
                log.info("添加模版参数:{},添加模版响应:{}", params, content);
                JSONObject tplContent = new JSONObject(content);
                WechatMessageUtil.wechatHandler(tplContent);

                //如果包含模版id则进行操作，否则抛出异常
                if (tplContent.containsKey("priTmplId")) {
                    wechatMPMessage.setCode(tplContent.getStr("priTmplId"));
                } else {
                    throw new ServiceException(ResultCode.WECHAT_MP_MESSAGE_TMPL_ERROR);
                }

                wechatMPMessage.setName(tplData.getSceneDesc());
                wechatMPMessage.setTemplateId(tplData.getTid());
                wechatMPMessage.setKeywords(JSONUtil.toJsonStr(tplData.getKeyWord()));
                wechatMPMessage.setKeywordsText(JSONUtil.toJsonStr(kidTexts));
                wechatMPMessage.setEnable(true);
                wechatMPMessage.setOrderStatus(tplData.getOrderStatus().name());
                this.save(wechatMPMessage);
            });
        } catch (Exception e) {
            log.error("初始化微信消息异常", e);
        }

    }


    public List<WechatMPMessageData> initData() {

        List<WechatMPMessageData> msg = new ArrayList<>();
        //支付提醒
        List<WechatMessageItemEnums> PAIDkeyWord = new ArrayList<>();
        PAIDkeyWord.add(WechatMessageItemEnums.ORDER_SN);
        PAIDkeyWord.add(WechatMessageItemEnums.MEMBER_NAME);
        PAIDkeyWord.add(WechatMessageItemEnums.PRICE);
        PAIDkeyWord.add(WechatMessageItemEnums.GOODS_INFO);
        msg.add(new WechatMPMessageData(
                "订单支付成功,准备发货",
                "487", PAIDkeyWord,
                OrderStatusEnum.UNDELIVERED));
        //发货提醒

        List<WechatMessageItemEnums> deliverdKeyWord = new ArrayList<>();
        deliverdKeyWord.add(WechatMessageItemEnums.ORDER_SN);
        deliverdKeyWord.add(WechatMessageItemEnums.GOODS_INFO);
        deliverdKeyWord.add(WechatMessageItemEnums.LOGISTICS_NAME);
        deliverdKeyWord.add(WechatMessageItemEnums.LOGISTICS_NO);
        msg.add(new WechatMPMessageData(
                "订单发货成功",
                "374", deliverdKeyWord,
                OrderStatusEnum.DELIVERED));


        //已完成

        List<WechatMessageItemEnums> completeKeyWord = new ArrayList<>();
        completeKeyWord.add(WechatMessageItemEnums.SHOP_NAME);
        completeKeyWord.add(WechatMessageItemEnums.GOODS_INFO);
        msg.add(new WechatMPMessageData(
                "订单完成",
                "3606", completeKeyWord,
                OrderStatusEnum.COMPLETED));

        return msg;


    }
}

@Data
@AllArgsConstructor
@NoArgsConstructor
class WechatMPMessageData {

    /**
     * 场景描述 // 等于本服务器模版名称
     */
    String sceneDesc;

    /**
     * 模版id
     */
    String tid;

    /**
     * 自身服务器 消息字段
     */
    List<WechatMessageItemEnums> keyWord;

    /**
     * 处于什么状态发送
     */
    OrderStatusEnum orderStatus;
}

@Data
class WechatMessageKeyword {
    /**
     * id字段
     */
    private String kid;
    /**
     * 名称
     */
    private String name;
    /**
     * 示例值
     */
    private String example;
    /**
     * 示例值
     */
    private String rule;
}