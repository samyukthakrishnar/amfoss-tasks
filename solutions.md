import 'dart:async';

import 'package:flutter/material.dart';
import 'dart:math';
import 'package:flutter/services.dart';
void main() {
  runApp(const MyApp());
}

class MyApp extends StatelessWidget {
  const MyApp({super.key});

  // This widget is the root of your application.
  @override
  Widget build(BuildContext context) {
    return MaterialApp(
      title: 'Word Game',
      theme: ThemeData(
        // This is the theme of your application.
        //
        // TRY THIS: Try running your application with "flutter run". You'll see
        // the application has a purple toolbar. Then, without quitting the app,
        // try changing the seedColor in the colorScheme below to Colors.green
        // and then invoke "hot reload" (save your changes or press the "hot
        // reload" button in a Flutter-supported IDE, or press "r" if you used
        // the command line to start the app).
        //
        // Notice that the counter didn't reset back to zero; the application
        // state is not lost during the reload. To reset the state, use hot
        // restart instead.
        //
        // This works for code too, not just values: Most code changes can be
        // tested with just a hot reload.
        colorScheme: ColorScheme.fromSeed(seedColor: const Color.fromARGB(255, 26, 158, 138)),
        useMaterial3: true,
      ),
      home: const WordGameScreen(title: 'Word Game',),
      debugShowCheckedModeBanner: false,
    );
  }
}

     class WordGameScreen extends StatefulWidget {
      final String title;
     const WordGameScreen({super.key, required this.title});

      // This widget is the home page of your application. It is stateful, meaning
       // that it has a State object (defined below) that contains fields that affect
      // how it looks.

      // This class is the configuration for the state. It holds the values (in this
     // case the title) provided by the parent (in this case the App widget) and
     // used by the build method of the State. Fields in a Widget subclass are
    // always marked "final".

  @override
  State<WordGameScreen> createState() => _WordGameScreenState();
}

class _WordGameScreenState extends State<WordGameScreen> {
  final List<String> words = [ 'TREE','GAME','FISH','BOOK','MILK','STAR','RING','FIRE','JUMP','WIND','PLANE','SUGAR','MOUSE','WATER','CHAIR','APPLE','GRASS','SMILE','CLOUD','BRAIN','DOOR','HAND','SNOW','BALL','NOTE','LION','ROAD','KING','MOON','BEAR','SHIP','CAKE','TIME','SAND','BIRD','COOK','DESK','FACE','HILL','LEAF'

  ];
    String currentlytheWord = '';
    List<String> scrambledalphabets = [];
    List<String> selectedalphabets = [];
    int score = 0;
    bool gameStarted = false;
    int highScore=0;
    Timer? _gameTimer;
    int timeLeft =20;
  
  @override
  void initState() {
    super.initState();
   startTimer();
  }
  void dispose() {
    _gameTimer?.cancel();
    super.dispose();
  }
  // ignore: non_constant_identifier_names
  void _startGame() {
    setState(() {
      gameStarted=true;
      score=0;
      timeLeft=20;
      _newRound();
      startTimer();
    });
  }
  void _newRound()  {
    _gameTimer?.cancel();
    setState(() {
      currentlytheWord=words[Random().nextInt(words.length)];
      scrambledalphabets=currentlytheWord.split('');
      scrambledalphabets.shuffle();
      selectedalphabets=[];
      _gameTimer?.cancel();
      timeLeft=20;
      startTimer();
    });
  }
  void startTimer() {
    _gameTimer?.cancel();
    _gameTimer= Timer.periodic(Duration(seconds:1),(timer){
      setState(() {
        timeLeft--;
        if(timeLeft >0){
          timeLeft--;
        }
        if(timeLeft==0){
          timer.cancel();
        __showGameOverDialog();
        }
      });
    });
  }
  void __showGameOverDialog(){
    showDialog(
      context: context,
      barrierDismissible: false,
    
     builder:(context)=>AlertDialog(
      title: Text('GAME OVER!'),
      content: Text('Time\ is up!\nThe word was"$currentlytheWord"\nFinal Score:$score\nHigh Score:$highScore'),
      actions: [
      TextButton(
      onPressed:() {
        Navigator.of(context).pop();
        setState(() {
          gameStarted = false;
          if (score>highScore){
          highScore=score;
          }
        });
      },
      child: Text('Try Again'),
      ),
      ],
     ),
     );
  }
  void _playSound(String soundType)async{
    try{
      switch(soundType){
        case 'tap':
        HapticFeedback.lightImpact();
        break;
        case'correct' :
        HapticFeedback.heavyImpact();
        break;
        case'wrong':
        HapticFeedback.vibrate();
        break;
      }
    }catch(e){print("Haptic feedback not available");}
  }
  // ignore: unused_element
  void _selectalphabets(int index) {
     _playSound('tap');
    setState(() {
      selectedalphabets.add(scrambledalphabets[index]);
      scrambledalphabets.removeAt(index);
      });
  }
  // ignore: unused_element
  void _deselectalphabets(int index) {
    _playSound('tap');
    setState(() {
      scrambledalphabets.add(selectedalphabets[index]);
      selectedalphabets.removeAt(index);
    });
  }
  void _checkAnswer(){
    String userWord=selectedalphabets.join('');
    if (userWord==currentlytheWord) {
      _playSound('correct');
      
      setState(() {
        score+=10;
        if (score< highScore){highScore= highScore;}
      });
      _showDialog('Correct!', 'You got "$currentlytheWord"!\n+10 points', true);
    } else {
      
      _playSound('wrong');
       score+=10;
       if (score> highScore){highScore= score;}
      _showDialog('GAME OVER!', 'The word was "$currentlytheWord"\nfinal Score:$score\nHigh Score: $highScore', false);
    }
  }
  void _showDialog(String title, String content, bool Correct) {
    showDialog(
      context:context,
      builder: (context) => AlertDialog(
        title: Text(title),
        content: Text(content),
        actions: [
          TextButton(
            onPressed: () {
               Navigator.of(context).pop();
               if (Correct) {
                  _newRound();
               } else {
                 setState(() {
                  gameStarted=false;
  });
               }
            },
            child: Text(Correct ?'Next Word' :'Try Again'),
          ),
        ],
      ),
    );
  }


  void _clearSelection() {
    setState(() {
      scrambledalphabets.addAll(selectedalphabets);
      selectedalphabets.clear();
    });
  }


  @override
  Widget build(BuildContext context) {
    if(!gameStarted) {
    return Scaffold(
        
        backgroundColor: Colors.black87,
        body: Center(
        child: Column(
          mainAxisAlignment: MainAxisAlignment.center,
          children: [
            Icon(
              Icons.wordpress_sharp,
              size:150,
              color: const Color.fromARGB(255, 66, 205, 177),
              ),
            SizedBox(height: 25,),
            Text(
              'Word Game',
            style: TextStyle(
              fontSize: 40,
              fontWeight: FontWeight.bold,
              color: const Color.fromARGB(255, 99, 186, 124),
            ),
        ),
            SizedBox(height: 45),
            ElevatedButton(
              onPressed:_startGame,
              style: ElevatedButton.styleFrom(
                backgroundColor: const Color.fromARGB(255, 97, 175, 127),
                foregroundColor: const Color.fromARGB(255, 249, 246, 246),
                padding: EdgeInsets.symmetric(horizontal:42, vertical:20)
              ),
              child: Text(
                'START GAME',
                style: TextStyle(fontSize: 20),
              ),
            ),
            SizedBox(height:22),
            Text('High Score: $score',
            
            style: TextStyle(fontSize: 18),
            selectionColor: Color.fromARGB(0, 239, 239, 242), 
            )
          ],
        ),
      ),
    );
  }
  return Scaffold(
    backgroundColor: Colors.black,
    appBar: AppBar(
      
    backgroundColor: const Color.fromARGB(255, 39, 48, 53),
    foregroundColor: Colors.white,
    title: Text('Score: $score'),
    centerTitle: true,
    leading: IconButton(
     icon: Icon(Icons.home),
     onPressed: () {
       setState(() {
         gameStarted=false;
         
       });
     },
  ),
    ),
    body: Padding(
      padding: const EdgeInsets.all(22.0),
      child: Column(
      children: [
        Text('Make a ${currentlytheWord.length}-letter word',
        style: TextStyle(fontSize: 20,fontWeight: FontWeight.bold),
        ),
        SizedBox(height: 35),
        Container(
          padding: EdgeInsets.all(16),
          child: Text('Time:$timeLeft',style: TextStyle(fontSize:18),)
        
        ),
        
        Container(
          height: 85,
          padding: EdgeInsets.all(12),
          decoration: BoxDecoration(
            color: Colors.white,
            borderRadius: BorderRadius.circular(10),
            border: Border.all( color: const Color.fromARGB(255, 147, 229, 247), width: 3 ),

          ),
        child: Row(
          mainAxisAlignment: MainAxisAlignment.center,
          children: selectedalphabets.asMap().entries.map((entry) {
            int index = entry.key;
            String letter =entry.value;
            
            return GestureDetector(
              onTap: () => _deselectalphabets(index),
              child: Container(
                margin: EdgeInsets.all(6),
                width: 55,
                height:55,
                decoration: BoxDecoration(
                  color: const Color.fromARGB(255, 56, 111, 71),
                  borderRadius: BorderRadius.circular(9),
                ),
                child: Center(
                  child: Text(
                    letter,
                    style: TextStyle(
                      fontSize: 25,
                      fontWeight:  FontWeight.bold,
                    ),
                  ),
                  ),
                ),

                );
  }).toList(),

            ),
            
          ),
          SizedBox(height: 32),
      Wrap(
        alignment: WrapAlignment.center,
        children: scrambledalphabets.asMap().entries.map((entry) {
          int index= entry.key;
          String letter= entry.value;
          return GestureDetector(
            onTap: ()=> _selectalphabets(index),
            child: Container(
              margin: EdgeInsets.all(9),
              width:60,
              height: 60,
              decoration: BoxDecoration(
                color: const Color.fromARGB(255, 117, 218, 149),
                borderRadius: BorderRadius.circular(10),
                boxShadow: [
                  BoxShadow(
                    color:Colors.black38,
                    blurRadius: 4,
                    offset: Offset(1, 3)
                  ),
                ],
              ),
              child: Center(
                child: Text(
                  letter,
                  style: TextStyle(
                    color: Colors.white,
                    fontSize: 30,
                    fontWeight: FontWeight.bold,
                  ),
                ),
              ),
            ),
          );
        }).toList(),
      ),

        Spacer(),
        Row(
          mainAxisAlignment: MainAxisAlignment.spaceEvenly,
          children: [
            ElevatedButton(onPressed: selectedalphabets.length==currentlytheWord.length
            ?_checkAnswer
            : null,
            style: ElevatedButton.styleFrom(
              backgroundColor: Colors.lightGreen,
              foregroundColor: Colors.grey
            ),
             child: Text('check'),
             ),
          ],
        ),
      ],
      ),
    ),
  );
  }
}

- The full code that I used to make my app
