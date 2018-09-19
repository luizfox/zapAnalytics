import { Component, OnInit } from '@angular/core';
import { HttpClient } from '@angular/common/http';
type Thing = {
    name: string;
    info?: string;
};
const SERVER = "http://localhost:8000";

@Component({
    selector: 'main',
    template: require('./main.html'),
    styles: [require('./main.scss')],
})
export class MainComponent implements OnInit {

    awesomeThings: Thing[] = [];
    newThing = '';
		echo = [];
		lonelyboys = [];
		terms = [];


    static parameters = [HttpClient];
    constructor(http: HttpClient) {
        this.http = http;
    }

    ngOnInit() {
			this.lonelyboysFN();
			this.termsFN();
    }

		graphics(number){
			var url = `${SERVER}/grafico${number}`;
			return url;
		}


		lonelyboysFN(){
			return this.http.get(`${SERVER}/lonelyboys`)
					.subscribe(answer => {
							this.lonelyboys = answer;
							this.lonelyboys.forEach(function(key){
								if (key.Var1.length > 10) {
									key.Var1 = key.Var1.substr(key.Var1.length - 10);
								}
							})
					},
					error => {console.log(error);}
				);
		}

		termsFN(){
			return this.http.get(`${SERVER}/findassociatedword?term=eleicoes`)
					.subscribe(answer => {
						this.terms = answer;
						this.terms = this.terms.slice (0,6);
						console.log(this.terms);},
										 error => {console.log(error);}
				);
		}

		echoFN(){
			return this.http.get('http://localhost:8000/echo')
					.subscribe(res => { this.echo = res; return res;},
										 error => {console.log(error);}
				);
	}
}
